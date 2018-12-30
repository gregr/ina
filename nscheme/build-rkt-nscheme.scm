((require program-path printf file-exists? read*/file write/file
          language:base module:base-primitive module:base premodule
          premodule:parse module:premodule module:compose
          module:ast->ast module:meta rkt:module ast-elaborate))

(define rpath:here (cdr (reverse program-path)))
(define rpath:lib  (cons 'lib rpath:here))
(define path:out   (reverse (cons 'nscheme.scm.rkt rpath:here)))
(define str:out
  (apply string-append
         (cons (car path:out) (map (lambda (s) (string-append '"/" s))
                                   (cdr path:out)))))

(when (file-exists? path:out)
  (printf '"~s already exists; remove it to rebuild it.\n" str:out))

(unless (file-exists? path:out)
  (define name=>lang (list (cons 'base language:base)))
  (define (module:file name)
    (module:premodule name=>lang (premodule:parse '(base) (read*/file name))))
  (define paths:lib-modules
    (map (lambda (n) (reverse (cons (string-append n '".scm") rpath:lib)))
         '(common ast parse module base backend-racket module-racket)))
  (define mtest
    (module:premodule name=>lang (premodule '() '(test) '() '(test) '(base)
                                            '((define test #f)))))
  ;; TODO: attach host prim descriptions.
  (define nscheme.scm.rkt
    (module:meta
      (module:compose
        #t (foldl (lambda (mnext m) (module:compose #f m mnext))
                  module:base-primitive
                  (list* module:base mtest
                         (map module:file paths:lib-modules)))
        (module:file (reverse (cons 'nscheme.scm rpath:here))))
      '((rkt:require #(string prim.rkt) ;; TODO: #(string host.rkt)
                     )
        (rkt:module-name _)
        (rkt:module-lang racket/base)
        (rkt:define-name main)
        (rkt:suffix (module+ main (main))))))
  (write/file path:out
              (rkt:module (module:ast->ast nscheme.scm.rkt ast-elaborate)))
  (printf '"Finished building: ~s\n" str:out))
