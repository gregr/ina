;; References:
;; O. Waddell et al
;;   Fixing Letrec: A Faithful Yet Efficient Implementation of Scheme's Recursive Binding Construct
;; https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf
;; A. Ghuloum et al
;;   Fixing Letrec (reloaded)
;; https://legacy.cs.indiana.edu/~dyb/pubs/letrec-reloaded.pdf
(define (simplify-letrec E)
  (let loop ((E E))
    (cond
      ((E:annotated?    E) (E:annotated (E:annotated-annotation E) (loop E)))
      ((E:quote?        E) E)
      ((E:ref?          E)
       )
      ((E:if?           E)
       )
      ((E:call?         E)
       )
      ((E:apply/values? E)
       )
      ((E:case-lambda?  E)
       )
      ((E:letrec?       E)

       partition bindings:
       - unreferenced
       - simple
       - lambda
       - complex

       )
      (else (error "not an expression" E)))))

;; Different treatment for complex bindings that are allocation/construction-only:
(letrec ((x (list (lambda () x)))) x)

;; no need to set! x, since it's better to closure-set! the captured x slot instead

;; Should we just desugar letrec into mvector boxed variables, and rely on the optimizer or a later pass to recover letrec-like bindings?
