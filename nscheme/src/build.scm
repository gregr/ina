(define cli-arg* (current-posix-argument*))
(define path.self (car cli-arg*))
(define path.library (path-directory path.self))
(define verbose? #t)
(define out.verbose (and verbose? (current-error-port)))
(define (verbose-write     . x*) (when verbose? (for-each (lambda (x) (pretty-write x out.verbose)) x*)))
(define (verbose-displayln . x*) (when verbose? (for-each (lambda (x) (displayln x out.verbose)) x*)))

(define library=>text* (posix-make-library=>text* out.verbose path.library))
(define library=>def* (make-library=>def* out.verbose #t library=>text*))
(define E.run-cli (parse-bootstrapped-program-definition* library=>text* library=>def* (alist-ref library=>def* 'run-cli)))

;; TODO: generate the complete code
;; - for now, we can generate a single nscheme .scm file, which should be directly runnable with run-file.rkt or run-cli.scm
;; - write the generated code as the file "../built/run-cli.scm"
(compact-write (E-pretty E.run-cli))
