(define library=>path*
  '((base
      "base/misc.scm"
      "base/pair.scm"
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
    (env "env.scm")
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
      "posix/terminal/osc.scm"
      "posix/terminal/csi.scm"
      "posix/terminal/sgr.scm"
      "posix/terminal/tty.scm"
      "posix/terminal/text.scm")))

(define env.common
  (value-package->env
    (cons
      '(
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
        integer-floor-divmod numerator denominator = <= >= < > + - * /)
      (list
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
        integer-floor-divmod numerator denominator = <= >= < > + - * /))))
(define env.control
  (value-package->env
    (cons
      '(
        make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
        current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
        sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
        make-channel channel-get channel-put channel-put-evt
        current-platform)
      (list
        make-parameter current-panic-handler current-custodian make-custodian custodian-shutdown-all
        current-thread-group make-thread-group current-thread thread thread-wait thread-dead-evt
        sync sync/default handle-evt choice-evt guard-evt nack-guard-evt replace-evt never-evt
        make-channel channel-get channel-put channel-put-evt
        current-platform))))
(define env.tiny  (env-conjoin* env.minimal env.common))
(define env.small (env-conjoin* env.tiny env.control))
(define library=>env.cross-phase
  (list (cons 'small   env.small)
        (cons 'tiny    env.tiny)
        (cons 'meta    env.meta)
        (cons 'syntax  env.syntax)
        (cons 'minimal env.minimal)
        (cons 'control env.control)
        (cons 'common  env.common)))

(define (make-library=>def* read-file)
  (map (lambda (lib&path*) (cons (car lib&path*) (append* (map read-file (cdr lib&path*)))))
       library=>path*))

(define (((make-load-library/library=>def* library=>def*) load-definition*) env lib)
  (load-definition* env (alist-ref library=>def* lib)))

(define (((make-load-library-lazy/read-file read-file) load-definition*) env lib)
  (load-definition* env (append* (map read-file (alist-ref library=>path* lib)))))

(define (make-library=>env load-library)
  (define env.base     (env-conjoin* env.small (load-library env.small 'base)))
  (define env.compiler (load-library env.base 'compiler))
  (define env.deps     (env-conjoin* env.base env.syntax env.compiler))
  (define env.parser   (load-library env.deps 'parser))
  (define env.medium   (env-conjoin* env.deps env.meta env.parser))
  (define env.posix    (load-library env.base 'posix))
  (define env.large    (env-conjoin* env.medium env.posix))
  (cons* (cons 'large    env.large)
         (cons 'posix    env.posix)
         (cons 'medium   env.medium)
         (cons 'parser   env.parser)
         (cons 'compiler env.compiler)
         (cons 'base     env.base)
         library=>env.cross-phase))

(define (make-library=>env/library=>def* library=>def* load-definition*)
  (make-library=>env ((make-load-library/library=>def* library=>def*) load-definition*)))

(define (posix-make-library=>def* path.here)
  (make-library=>def* (lambda (p) (posix-read-file (path-append path.here p)))))
