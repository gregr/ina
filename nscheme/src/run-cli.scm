(define options
  `(((flags "-h" "--help")
     (description "Print this usage and option information, then exit.")
     ,(lambda (_) (display-usage (current-output-port)) (exit 0)))
    ((flags "-q" "--quiet")
     (description "Suppress implicit printing when loading definitions.")
     ,(lambda (arg*) (set! quiet? #t) (loop arg*)))
    ((flags "-v" "--verbose")
     (description "Print diagnostic information during startup.")
     ,(lambda (arg*) (set! verbose? #t) (loop arg*)))
    ((flags "-i" "--interact")
     (description "Load definitions interactively from standard input after loading all other sources.")
     ,(lambda (arg*) (set! interact? #t) (loop arg*)))
    ((flags "-f" "--file")
     (description "Load definitions from <file>.")
     (arguments "<file>")
     ,(lambda (path arg*) (source*-add-file! path) (loop arg*)))
    ((flags "-t" "--text")
     (description "Load <definitions> from text provided directly on the command-line.")
     (arguments "<definitions>")
     ,(lambda (text.def* arg*) (source*-add-text! text.def*) (loop arg*)))
    ((flags "-c" "--compile")
     (description
       "Compile loaded definitions instead of evaluating them."
       "Generate code for <target> and write the generated code to <output-file>."
       "<target> must be one of the following:"
       "  racket"
       "<output-file> may be '-' to write the generated code to standard output."
       "This option may be used multiple times to compile for multiple targets at once.")
     (arguments "<target>" "<output-file>")
     ,(lambda (target path.out arg*) (compiler-output*-add! target path.out) (loop arg*)))
    ((flags "-")
     (description "Pass through remaining command-line arguments.")
     ,(lambda (arg*) (finish 'stdin arg*)))
    ((flags "--")
     (description "Stop parsing option flags in remaining command-line arguments.")
     ,(lambda (arg*) (finish #f arg*)))))

(define (display-usage out)
  (displayln (usage-description path.self #"[<option> ...] [(<file> | -) <argument> ...]" options) out))

(define (usage-error . desc*)
  (let ((out (current-error-port)))
    (displayln "usage error:" out)
    (for-each (lambda (desc) (displayln desc out)) desc*)
    (newline out)
    (display-usage out)
    (exit 1)))

(mdefine cli-arg* (current-posix-argument*))
(define path.self (car cli-arg*))

(mdefine quiet? #f)
(mdefine verbose? #f)
(mdefine interact? #f)
(mdefine compiler-output* '())
(mdefine source* '())
(define (compiler-output*-add! target path)
  (let ((target (bytevector->symbol target)))
    (unless (memv target '(racket))
      (usage-error "not a supported compile target" `(<target> ,target) `(<output-file> ,path)))
    (set! compiler-output* (cons (cons target path) compiler-output*))))
(define (source*-add! src) (set! source* (cons src source*)))
(define (source*-add-file! path) (source*-add! `(file ,path)))
(define (source*-add-text! txt)  (source*-add! `(text ,txt)))
(define (source*->path=>def*)
  (if (null? source*)
      (verbose-displayln "No additional sources to load.")
      (verbose-displayln "Reading sources:"))
  (map (lambda (source)
         (case (car source)
           ((file)  (let ((path (cadr source)))
                      (verbose-write `(read-file ,path))
                      (cons path (posix-read-file-annotated path))))
           ((text)  (let ((text (cadr source)))
                      (verbose-write `(read-text ,text))
                      (cons #f ((read*-syntax-annotated/source 'cli) (iport:bytevector text)))))
           ((stdin) (cons #f ((read*-syntax-annotated/source 'stdin) (current-input-port))))
           (else (mistake "unexpected definition source" source))))
       source*))
(define (source*->path=>text)
  (if (null? source*)
      (verbose-displayln "No additional sources to load.")
      (verbose-displayln "Reading sources:"))
  (map (lambda (source)
         (case (car source)
           ((file)  (let ((path (cadr source)))
                      (verbose-write `(read-file-text ,path))
                      (cons path (file->bytevector path))))
           ((text)  (let ((text (cadr source)))
                      (verbose-write `(read-text ,text))
                      (cons 'cli text)))
           ((stdin) (cons 'stdin (iport->bytevector (current-input-port))))
           (else (mistake "unexpected definition source" source))))
       source*))

(define dispatch (options->dispatch options))
(define (loop arg*) (dispatch arg* (lambda () (finish #f arg*))))
(define (finish stdin? arg*)
  (cond (stdin?       (unless interact? (source*-add! '(stdin)))
                      (set! cli-arg* (cons path.self arg*)))
        ((pair? arg*) (source*-add-file! (car arg*))
                      (set! cli-arg* arg*))
        (else         (when (null? source*) (set! interact? #t))
                      (set! cli-arg* (list path.self))))
  (set! source* (reverse source*)))

(loop (cdr cli-arg*))
(define out.verbose (and verbose? (current-error-port)))
(define (verbose-write . x*)
  (when verbose? (for-each (lambda (x) (pretty-write x out.verbose)) x*)))
(define (verbose-displayln . x*)
  (when verbose? (for-each (lambda (x) (displayln x out.verbose)) x*)))
(verbose-displayln
  "This program uses a persisted copy of the library file data that it was built with."
  "Rebuild this program to update the library files it uses.")
(apply verbose-write
       `((quiet? ,quiet?)
         (verbose? ,verbose?)
         (interact? ,interact?)
         (command-line-arguments ,cli-arg*)
         (definition-sources . ,source*)
         (compiler-outputs . ,compiler-output*)
         (library-files . ,library=>path*)))
(define library=>def* (make-library=>def* out.verbose #t library=>text*))

(define current-include-directory (make-parameter #f))
(define env.import
  (let ((env.import (make-env)))
    (define (parse-load env.d env stx.path)
      (let ((path (syntax-unwrap stx.path)))
        (unless (text? path) (raise-parse-error (list 'load "not a path") stx.path))
        (parse-begin-definition* env.d env (posix-read-file-annotated path))))
    (env-vocabulary-bind!
      env.import 'load vocab.definition-operator (definition-operator-parser parse-load 1 1))
    (define (parse-include env.d env stx.path)
      (let ((path (syntax-unwrap stx.path)))
        (unless (text? path) (raise-parse-error (list 'include "not a path") stx.path))
        (let* ((path (let ((dir (current-include-directory)))
                       (if dir (path-append dir path) path)))
               (def* (posix-read-file-annotated path)))
          (current-include-directory
            (path-directory path)
            (lambda () (parse-begin-definition* env.d env def*))))))
    (env-vocabulary-bind!
      env.import 'include vocab.definition-operator (definition-operator-parser parse-include 1 1))
    (env-freeze env.import)))

(if (null? compiler-output*)
    (let* ((library=>env (make-library=>env out.verbose library=>text* library=>def*))
           (env (env-conjoin (alist-ref library=>env 'large) env.import))
           (path=>def* (source*->path=>def*)))
      (define eval-def*
        (if quiet?
            eval-definition*
            (eval-definition*/yield (lambda x* (for-each pretty-write x*)))))
      (current-posix-argument*
        cli-arg*
        (lambda ()
          (verbose-displayln
            (string-append "Loading source definitions: "
                           (number->string (length (append* (map cdr path=>def*))))))
          (let ((env (let loop ((path=>def* path=>def*) (env env))
                       (if (null? path=>def*)
                           env
                           (let* ((path&def* (car path=>def*))
                                  (path      (car path&def*)))
                             (loop (cdr path=>def*)
                                   (env-conjoin
                                     (current-include-directory
                                       (and path (path-directory path))
                                       (lambda () (eval-def* env (cdr path&def*))))
                                     env)))))))
            (when interact?
              (unless quiet?
                (displayln "Entering interactive evaluator.  (exit) or Ctrl-d to exit."))
              (let ((ch.command (make-channel)))
                (posix-set-signal-handler! SIGINT (lambda (sig) (channel-put ch.command retry)))
                (let loop ((env env))
                  (with-retry
                    '(abort current evaluation and return to prompt)
                    (lambda ()
                      (current-panic-handler
                        (lambda x*
                          (displayln "unhandled panic:")
                          (for-each (lambda (x) (if (text? x) (displayln x) (pretty-write x))) x*)
                          (retry))
                        (lambda ()
                          (unless quiet? (displayln ";; Evaluate:"))
                          (thread
                            (lambda ()
                              (case-values (read)
                                (()    (exit))
                                ((stx) (let ((env.new (eval-def* env (list stx))))
                                         (channel-put ch.command (lambda () (loop (env-conjoin env.new env)))))))))
                          ((channel-get ch.command)))))))))))))
    (let ((E.program
            (parse-bootstrapped-program-definition*
              library=>text* library=>def*
              `((let* ((library=>text* ',library=>text*)
                       (library=>def* (make-library=>def* #f #t library=>text*))
                       (library=>env (make-library=>env #f library=>text* library=>def*))
                       (env (alist-ref library=>env 'large))
                       (program (make-program))
                       (def*.source*
                         (append*
                           (alist-map
                             ',(source*->path=>text)
                             (lambda (path text)
                               ((text->definition*/read*
                                  (if path (read*-syntax-annotated/source path) read*-syntax))
                                text))))))
                  (program-parse-definition* program env def*.source*)
                  (E-eval (program->E program)))))))
      (alist-for-each
        (reverse compiler-output*)
        (lambda (target path)
          (verbose-displayln "Compiling:" '<target> target '<output-file> path)
          (define (write-output text)
            (if (eqv? path #"-")
                (displayln text)
                (call/oport:file path 'create (lambda (out) (displayln text out)))))
          (case target
            ((racket) (write-output (E-compile-racket-program E.program)))
            (else (mistake "compiler target is not yet supported" target path)))))))
