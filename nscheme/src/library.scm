(define library=>path*
  '((base
      "base/misc.scm"
      "base/list.scm"
      "base/number.scm"
      "base/mvector.scm"
      "base/vector.scm"
      "base/mbytevector.scm"
      "base/bytevector.scm"
      "base/unicode.scm"
      "base/prompt.scm"
      "base/exception.scm"
      "base/coroutine.scm"
      "base/generator.scm"
      "base/port.scm"
      "base/text.scm"
      "base/platform.scm"
      "base/time.scm"
      "base/io.scm")
    (syntax "syntax.scm")  ; typically, we cross-phase persist this
    (compiler
      "compiler/high-level-ir.scm"
      "compiler/high-level-passes.scm"
      "compiler/backend/rkt.scm"
      "compiler/target/racket.scm")
    (parser
      "parser/parse.scm"
      "parser/minimal.scm"
      "parser/program.scm"
      "parser/meta.scm")
    (posix
      "posix/platform.scm"
      "posix/signal.scm"
      "posix/filesystem.scm"
      "posix/network.scm"
      "posix/process.scm"
      "posix/cli.scm"
      "posix/terminal/osc.scm"
      "posix/terminal/csi.scm"
      "posix/terminal/sgr.scm"
      "posix/terminal/tty.scm"
      "posix/terminal/text.scm")
    (extended
      "extended/meta.scm"
      "extended/match.scm"
      "extended/record.scm")
    (library "library.scm")
    (build "build.scm")
    (run-cli "run-cli.scm")))

(define env.common
  (value-alist->env
    (aquote
      panic apply values make-record-type
      eqv? null? boolean? procedure? symbol? string? rational? integer?
      pair? vector? mvector? bytevector? mbytevector?
      bytevector->string string->bytevector string->symbol symbol->string
      cons car cdr vector vector-length vector-ref
      make-mvector mvector->vector mvector-length mvector-ref mvector-set!
      bytevector bytevector-length bytevector-ref
      make-mbytevector mbytevector->bytevector mbytevector-length mbytevector-ref mbytevector-set!
      bitwise-asl bitwise-asr bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length
      integer-floor-divmod numerator denominator = <= >= < > + - * /)))
(define env.control
  (value-alist->env
    (aquote
      make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
      current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
      sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
      make-channel channel-get channel-put channel-put-evt
      current-platform)))
(define env.nano  (env-conjoin env.minimal env.common))
(define env.micro (env-conjoin env.nano env.control))

(define read*-syntax (read*/reader:data ((reader:data-track-line/start 0) reader:data)))
(define (read*-syntax-annotated/source source)
  (read*/reader:data
    ((reader:data-track-line/start 0)
     (reader:data/annotate
       (lambda (x loc text loc.end text.end)
         (syntax-note-set x (vector source loc text loc.end text.end)))))))
(define ((text->definition*/read* read*) text) (read* (iport:bytevector text)))

(define ((posix-read-file/annotate? annotate?) path)
  (call-with-iport:file
    path
    (lambda (in)
      (if annotate?
          ((read*-syntax-annotated/source path) in)
          (read*-syntax in)))))
(define posix-read-file           (posix-read-file/annotate? #f))
(define posix-read-file-annotated (posix-read-file/annotate? #t))

(define (make-library=>text* out.verbose path->text)
  (define (p->text p)
    (when out.verbose (pretty-write `(read-file-text ,p) out.verbose))
    (path->text p))
  (alist-map-value library=>path* (lambda (path*) (map p->text path*))))

(define (posix-make-library=>text* out.verbose path.library)
  (make-library=>text* out.verbose (lambda (p) (file->bytevector (path-append path.library p)))))

(define (make-library=>def* out.verbose annotate? library=>text*)
  (when out.verbose (displayln "Reading library file definitions:" out.verbose))
  (define (path&text->def* path text)
    (when out.verbose (pretty-write `(read-file-definitions ,path) out.verbose))
    ((text->definition*/read* (if annotate? (read*-syntax-annotated/source path) read*-syntax)) text))
  (map (lambda (lib path* text*) (cons lib (append* (map path&text->def* path* text*))))
       (map car library=>path*) (map cdr library=>path*) (map cdr library=>text*)))

(define (make-library=>env out.verbose library=>text* library=>def*)
  (when out.verbose (displayln "Loading libraries:" out.verbose))
  (define (load-library env lib)
    (when out.verbose (pretty-write `(load-library ,lib) out.verbose))
    (eval-definition* env (alist-ref library=>def* lib)))
  (let* ((env.base     (load-library env.micro 'base))
         (env.tiny     (env-conjoin env.micro env.base))
         (env.posix    (load-library env.tiny 'posix))
         (env.small    (env-conjoin env.tiny env.posix))
         (env.compiler (load-library env.tiny 'compiler))
         (env.parser   (load-library (env-conjoin env.tiny env.syntax env.compiler) 'parser))
         (env.medium   (env-conjoin env.small env.parser env.syntax env.compiler))
         (env.extended (load-library (env-conjoin env.medium env.meta) 'extended))
         (env.library  (load-library env.medium 'library))
         (menv.persist (make-env))
         (env.large    (env-conjoin env.extended env.medium env.meta env.library menv.persist))
         (library=>env
           (list (cons 'large    env.large)
                 (cons 'medium   env.medium)
                 (cons 'small    env.small)
                 (cons 'tiny     env.tiny)
                 (cons 'micro    env.micro)
                 (cons 'nano     env.nano)
                 (cons 'library  env.library)
                 (cons 'extended env.extended)
                 (cons 'parser   env.parser)
                 (cons 'compiler env.compiler)
                 (cons 'posix    env.posix)
                 (cons 'base     env.base)
                 (cons 'meta     env.meta)
                 (cons 'syntax   env.syntax)
                 (cons 'minimal  env.minimal)
                 (cons 'control  env.control)
                 (cons 'common   env.common))))
    (env-add-value-alist! menv.persist (aquote library=>text* library=>env))
    (env-read-only! menv.persist)
    library=>env))

(define (parse-bootstrapped-program-definition* library=>text* library=>def* def*.program)
  (define def*.library (append* (map (lambda (lib) (alist-ref library=>def* lib))
                                     '(base syntax compiler parser posix library))))
  (define program (make-program))
  (let ((env (env-conjoin (program-parse-definition* program env.micro def*.library)
                          env.micro
                          (value-alist->env (aquote library=>text*)))))
    (program-parse-definition* program env def*.program))
  (program->E program))
