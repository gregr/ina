(define (case-lambda-clause param body) (vector param body))
(define (case-lambda-clause-param cc)   (vector-ref cc 0))
(define (case-lambda-clause-body  cc)   (vector-ref cc 1))

(define (binding-pair left right)  (cons left right))
(define (binding-pair-left  bpair) (car bpair))
(define (binding-pair-right bpair) (cdr bpair))

(define (provenance-combine pv1 pv2)
  (cond ((not pv1)     pv2)
        ((not pv2)     pv1)
        ((eq? pv1 pv2) pv1)
        (else          (cons pv1 pv2))))

(define (ast-tag            ast)     (vector-ref ast 0))
(define (ast-provenance     ast)     (vector-ref ast 1))
(define (ast-provenance-set ast pv)  (let ((parts (vector->list ast)))
                                       (list->vector (cons (car parts) (cons pv (cddr parts))))))
(define (ast-provenance-add ast pv)  (ast-provenance-set
                                       ast (provenance-combine pv (ast-provenance ast))))
(define (ast-tagged?        ast tag) (eq? (ast-tag ast) tag))

(define (fresh-address name) (vector name))

;; TODO: ASTs for lower-level language integration:
;; - ast:unchecked-call, ast:let with type info, ast:case-lambda with type info, etc.
(define (ast:prim        pv name)              (vector 'prim        pv name))
(define (ast:quote       pv v)                 (vector 'quote       pv v))
(define (ast:ref         pv address)           (vector 'ref         pv address))
(define (ast:if          pv ast.c ast.t ast.f) (vector 'if          pv ast.c ast.t ast.f))
(define (ast:call        pv ast.proc ast.args) (vector 'call        pv ast.proc ast.args))
(define (ast:case-lambda pv clause*)           (vector 'case-lambda pv clause*))
(define (ast:letrec      pv bpair* body)       (vector 'letrec      pv bpair* body))

(define (ast:prim?        ast) (ast-tagged? ast 'prim))
(define (ast:quote?       ast) (ast-tagged? ast 'quote))
(define (ast:ref?         ast) (ast-tagged? ast 'ref))
(define (ast:if?          ast) (ast-tagged? ast 'if))
(define (ast:call?        ast) (ast-tagged? ast 'call))
(define (ast:case-lambda? ast) (ast-tagged? ast 'case-lambda))
(define (ast:letrec?      ast) (ast-tagged? ast 'letrec))

(define (ast:prim-name            ast) (vector-ref ast 2))
(define (ast:quote-value          ast) (vector-ref ast 2))
(define (ast:ref-address          ast) (vector-ref ast 2))
(define (ast:if-condition         ast) (vector-ref ast 2))
(define (ast:if-consequent        ast) (vector-ref ast 3))
(define (ast:if-alternative       ast) (vector-ref ast 4))
(define (ast:call-procedure       ast) (vector-ref ast 2))
(define (ast:call-argument*       ast) (vector-ref ast 3))
(define (ast:case-lambda-clause*  ast) (vector-ref ast 2))
(define (ast:letrec-binding-pair* ast) (vector-ref ast 2))
(define (ast:letrec-body          ast) (vector-ref ast 3))
