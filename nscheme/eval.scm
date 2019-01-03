((require program-arguments ns:nscheme read*/file printf
          language:empty language:initial language:primitive
          language:base-primitive language:base language:extended language:io
          premodule:parse module:premodule module-apply namespace-link*))

(define name=>lang (list (cons 'empty          language:empty)
                         (cons 'initial        language:initial)
                         (cons 'primitive      language:primitive)
                         (cons 'base-primitive language:base-primitive)
                         (cons 'base           language:base)
                         (cons 'extended       language:extended)
                         (cons 'io             language:io)))
(define (module:file path)
  (module:premodule name=>lang (premodule:parse '(base) (read*/file path))))
(define (eval/file path args)
  (define ns (cons (cons 'program-arguments (cons path args)) ns:nscheme+))
  (module-apply (module:file path) ns))
(define ns:nscheme+ (list* (cons 'eval/file eval/file)
                           (cons 'ns:nscheme ns:nscheme) ns:nscheme))
(define path:nscheme  (car program-arguments))
(define args          (cdr program-arguments))
(define rpath:here    (cdr (reverse path:nscheme)))
(cond ((null? args) (define pname (string-join path:nscheme '"/"))
                    (printf '"Usage: ~s PROGRAM [ARGUMENT]...\n" pname))
      (#t
       (define str:program   (car args))
       (define rpath:program (reverse (string-split str:program '"/")))
       (define path:program  (reverse (append rpath:program rpath:here)))
       (eval/file path:program (cdr args))))
