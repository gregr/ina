((require program-arguments printf file-exists? read*/file write/file
          language:empty language:initial language:primitive
          language:base-primitive language:base language:extended language:io
          module:base-primitive module:base module:io premodule premodule:parse
          module:premodule module:compose module:ast->ast module:meta
          module-provide rkt:module ast-elaborate))

(define program-path (car program-arguments))
(define rpath:here   (cdr (reverse program-path)))
(define rpath:lib    (cons 'lib rpath:here))
(define path:out     (reverse (cons 'nscheme.scm.rkt rpath:here)))
(define str:out
  (apply string-append
         (cons (car path:out) (map (lambda (s) (string-append '"/" s))
                                   (cdr path:out)))))

(when (file-exists? path:out)
  (printf '"~s already exists; remove it to rebuild it.\n" str:out))

(unless (file-exists? path:out)
  (define (module:compose* ms)
    (foldl (lambda (mnext m) (module:compose #f m mnext)) (car ms) (cdr ms)))
  (define name=>lang (list (cons 'empty          language:empty)
                           (cons 'initial        language:initial)
                           (cons 'primitive      language:primitive)
                           (cons 'base-primitive language:base-primitive)
                           (cons 'base           language:base)
                           (cons 'extended       language:extended)
                           (cons 'io             language:io)))
  (define (module:file name)
    (module:premodule name=>lang (premodule:parse '(base) (read*/file name))))
  (define paths:lib-modules
    (map (lambda (n) (reverse (cons (string-append n '".scm") rpath:lib)))
         '(common ast parse module base extended io
                  backend-racket module-racket)))
  (define mtest
    (module:premodule name=>lang (premodule '() '(test) '() '(test) '(base)
                                            '((define test #f)))))
  (define m:nscheme
    (module:compose* (list* module:base-primitive module:base mtest module:io
                            (map module:file paths:lib-modules))))
  (define names:nscheme (module-provide m:nscheme))
  (define body:ns:nscheme
    (list 'define 'ns:nscheme (list 'map 'cons (list 'quote names:nscheme)
                                    (cons 'list names:nscheme))))
  (define m:ns:nscheme
    (module:premodule
      name=>lang (premodule names:nscheme '(ns:nscheme) names:nscheme
                            '(ns:nscheme) '(initial) (list body:ns:nscheme))))
  (define m:args  ;; Guarantee that program-arguments will be a parameter.
    (module:premodule
      name=>lang (premodule '(program-arguments) '()
                            '(program-arguments) '() '(empty) '())))
  (define nscheme.scm.rkt
    (module:meta
      (module:compose
        #t (module:compose* (list m:nscheme m:ns:nscheme m:args))
        (module:file (reverse (cons 'nscheme.scm rpath:here))))
      '((rkt:require #(string prim.rkt) #(string io.rkt)
                     (only-in #(string interop.rkt) path:s->ns))
        (rkt:module-name _)
        (rkt:module-lang racket/base)
        (rkt:define-name main)
        (rkt:suffix
          (module+ main
            (define a* (cons (path:s->ns (find-system-path 'run-file))
                             (vector->list (current-command-line-arguments))))
            (void (main a*)))))))
  (write/file path:out
              (rkt:module (module:ast->ast nscheme.scm.rkt ast-elaborate)))
  (printf '"Finished building: ~s\n" str:out))
