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
      "compiler/backend/rkt.scm")
    (parser
      "parser/stage.scm"
      "parser/parse.scm"
      "parser/minimal.scm"
      "parser/match.scm"
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
    (bootstrap "bootstrap.scm")
    (run-cli "run-cli.scm")))

(define env.common
  (value-alist->env
    (aquote
      panic apply values make-record-type
      eqv? null? boolean? procedure? symbol? string? rational? integer?
      pair? vector? mvector? bytevector? mbytevector?
      bytevector->string string->bytevector string->symbol symbol->string
      cons car cdr
      vector vector-length vector-ref
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
(define env.nano  (env-conjoin* env.minimal env.common))
(define env.micro (env-conjoin* env.nano env.control))

(define (make-library=>env reboot? load-library)
  (let ((env.syntax.0 env.syntax))
    (define env.base      (load-library env.micro 'base))
    (define env.tiny      (env-conjoin* env.micro env.base))
    (define env.posix     (load-library env.tiny 'posix))
    (define env.small     (env-conjoin* env.tiny env.posix env.meta))
    (define env.syntax    (if reboot? (load-library env.tiny 'syntax) env.syntax.0))
    (define env.compiler  (load-library env.tiny 'compiler))
    (define env.parser    (load-library (env-conjoin* env.tiny env.syntax env.compiler) 'parser))
    (define env.medium    (env-conjoin* env.small env.parser env.syntax env.compiler))
    (define env.bootstrap (load-library env.medium 'bootstrap))
    (define env.large     (env-conjoin* env.medium env.bootstrap))
    (cons* (cons 'large     env.large)
           (cons 'medium    env.medium)
           (cons 'small     env.small)
           (cons 'tiny      env.tiny)
           (cons 'micro     env.micro)
           (cons 'nano      env.nano)
           (cons 'minimal   env.minimal)
           (cons 'meta      env.meta)
           (cons 'bootstrap env.bootstrap)
           (cons 'parser    env.parser)
           (cons 'compiler  env.compiler)
           (cons 'syntax    env.syntax)
           (cons 'posix     env.posix)
           (cons 'base      env.base)
           (cons 'control   env.control)
           (cons 'common    env.common))))

(define (make-library=>def* read-file)
  (map (lambda (lib&path*) (cons (car lib&path*) (append* (map read-file (cdr lib&path*)))))
       library=>path*))

(define ((make-load-library/library=>def* out.verbose library=>def* load-definition*) env lib)
  (when out.verbose (pretty-write `(load-library ,lib) out.verbose))
  (load-definition* env (alist-ref library=>def* lib)))

(define (((make-load-library-lazy/read-file read-file) load-definition*) env lib)
  (load-definition* env (append* (map read-file (alist-ref library=>path* lib)))))

(define (make-library=>env/library=>def* reboot? out.verbose library=>def* load-definition*)
  (make-library=>env reboot? (make-load-library/library=>def* out.verbose library=>def* load-definition*)))

(define read*-syntax (read*/reader:data ((reader:data-track-line/start 0) reader:data)))
(define (read*-syntax-annotated/source source)
  (read*/reader:data
    ((reader:data-track-line/start 0)
     (reader:data/annotate
       (lambda (x loc text loc.end text.end)
         (syntax-note-set x (vector source loc text loc.end text.end)))))))

(define ((posix-read-file/annotate? annotate?) path)
  (call-with-iport:file
    path
    (lambda (in)
      (if annotate?
          ((read*-syntax-annotated/source path) in)
          (read*-syntax in)))))
(define posix-read-file           (posix-read-file/annotate? #f))
(define posix-read-file-annotated (posix-read-file/annotate? #t))

(define (posix-make-library=>def* out.verbose path.here)
  (make-library=>def* (lambda (p)
                        (when out.verbose (pretty-write `(read-file ,p) out.verbose))
                        (posix-read-file (path-append path.here p)))))
