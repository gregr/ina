(define (case-lambda-clause param body) (vector param body))
(define (case-lambda-clause-param cc)   (vector-ref cc 0))
(define (case-lambda-clause-body  cc)   (vector-ref cc 1))

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
(define (ast:prim        pv name)              (vector 'E:prim        pv name))
(define (ast:quote       pv v)                 (vector 'E:quote       pv v))
(define (ast:ref         pv address)           (vector 'E:ref         pv address))
(define (ast:if          pv ast.c ast.t ast.f) (vector 'E:if          pv ast.c ast.t ast.f))
(define (ast:call        pv ast.proc ast.args) (vector 'E:call        pv ast.proc ast.args))
(define (ast:case-lambda pv clause*)           (vector 'E:case-lambda pv clause*))
(define (ast:letrec      pv lhs* rhs* body)    (vector 'E:letrec      pv lhs* rhs* body))

(define (ast:prim?        ast) (ast-tagged? ast 'E:prim))
(define (ast:quote?       ast) (ast-tagged? ast 'E:quote))
(define (ast:ref?         ast) (ast-tagged? ast 'E:ref))
(define (ast:if?          ast) (ast-tagged? ast 'E:if))
(define (ast:call?        ast) (ast-tagged? ast 'E:call))
(define (ast:case-lambda? ast) (ast-tagged? ast 'E:case-lambda))
(define (ast:letrec?      ast) (ast-tagged? ast 'E:letrec))

(define (ast:prim-name             ast) (vector-ref ast 2))
(define (ast:quote-value           ast) (vector-ref ast 2))
(define (ast:ref-address           ast) (vector-ref ast 2))
(define (ast:if-condition          ast) (vector-ref ast 2))
(define (ast:if-consequent         ast) (vector-ref ast 3))
(define (ast:if-alternative        ast) (vector-ref ast 4))
(define (ast:call-procedure        ast) (vector-ref ast 2))
(define (ast:call-argument*        ast) (vector-ref ast 3))
(define (ast:case-lambda-clause*   ast) (vector-ref ast 2))
(define (ast:letrec-binding-left*  ast) (vector-ref ast 2))
(define (ast:letrec-binding-right* ast) (vector-ref ast 3))
(define (ast:letrec-body           ast) (vector-ref ast 4))
