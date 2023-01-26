(define (case-lambda-clause param body) (vector param body))
(define (case-lambda-clause-param cc)   (vector-ref cc 0))
(define (case-lambda-clause-body  cc)   (vector-ref cc 1))

(define (binding-pair lhs rhs)   (cons lhs rhs))
(define (binding-pair-lhs bpair) (car bpair))
(define (binding-pair-rhs bpair) (cdr bpair))

(define (provenance-combine pv1 pv2)
  (cond ((not pv1)     pv2)
        ((not pv2)     pv1)
        ((eq? pv1 pv2) pv1)
        (else          (cons pv1 pv2))))

(define (ast-provenance     ast)    (vector-ref ast 1))
(define (ast-provenance-set ast pv) (let ((parts (vector->list ast)))
                                      (list->vector (cons (car parts) (cons pv (cddr parts))))))
(define (ast-provenance-add ast pv) (ast-provenance-set
                                      ast (provenance-combine pv (ast-provenance ast))))

(define (fresh-address name) (vector name))

(define (ast:prim        pv name)              `#(prim        ,pv ,name))
(define (ast:quote       pv v)                 `#(quote       ,pv ,v))
(define (ast:ref         pv address)           `#(ref         ,pv ,address))
(define (ast:if          pv ast.c ast.t ast.f) `#(if          ,pv ,ast.c ,ast.t ,ast.f))
(define (ast:call        pv ast.proc ast.args) `#(call        ,pv ,ast.proc ,ast.args))
(define (ast:case-lambda pv clause*)           `#(case-lambda ,pv ,clause*))
(define (ast:letrec      pv bpair* body)       `#(letrec      ,pv ,bpair* ,body))
