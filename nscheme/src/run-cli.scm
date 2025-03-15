(define cli-arg* (current-posix-argument*))
(define path.library (path-directory (car cli-arg*)))

(define quiet? #t)
(define verbose? #t)
(define verbose-write
  (if verbose?
      (let ((out (current-error-port)))
        (lambda x* (for-each (lambda (x) (pretty-write x out)) x*)))
      (lambda x* (values))))
(define verbose-display
  (if verbose?
      (let ((out (current-error-port)))
        (lambda x* (for-each (lambda (x) (displayln x out)) x*)))
      (lambda x* (values))))

(verbose-write
  `(command-line ,cli-arg*)
  `(library-path ,path.library))

(define interact? #t)
(define path.program #f)

(verbose-write
  `(interact? ,interact?)
  `(program-path ,path.program))

(define library=>def* (posix-make-library=>def* path.library))
(define library=>env  (make-library=>env/library=>def* library=>def* eval-definition*))

(let ((env (alist-ref library=>env 'large)))
  (current-posix-argument*
    (cdr cli-arg*)
    (lambda ()
      (let ((env (if path.program
                     (env-conjoin (eval-definition* env (posix-read-file path.program)) env)
                     env)))
        (when interact?
          (unless quiet? (displayln "Entering REPL"))
          (let ((eval-def* (if quiet?
                               eval-definition*
                               (eval-definition*/yield
                                 (lambda x* (for-each (lambda (x)
                                                        (display "; ")
                                                        (pretty-write x))
                                                      x*))))))
            ;; TODO: panic handling, abort, retry
            (let loop ((env env))
              (unless quiet? (displayln ";; evaluate:"))
              (case-values (read)
                (()    (values))
                ((stx) (loop (env-conjoin (eval-def* env (list stx)) env)))))))))))
