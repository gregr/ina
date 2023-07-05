(define (identifier->fresh-address p) (fresh-address (syntax-peek p)))

(define ($provenance  pv E) (ast-provenance-add E pv))
(define ($prim        name) (ast:prim  #f name))
(define ($quote      value) (ast:quote #f value))
(define ($ref         addr) (ast:ref   #f addr))
(define ($call proc . args) (ast:call  #f proc args))
(define ($if         c t f) (ast:if    #f c t f))

(define ($case-lambda . cc*)
  (define ($case-lambda-clause param*~ arg*->body)
    (let* ((addr*~ (improper-list-map identifier->fresh-address param*~)))
      (case-lambda-clause addr*~ (apply arg*->body (map $ref (improper-list->list addr*~))))))
  (ast:case-lambda #f (map (lambda (cc) ($case-lambda-clause (car cc) (cdr cc))) cc*)))

(define ($letrec param* ^rhs*&body)
  (let ((addr* (map identifier->fresh-address param*)))
    (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
      (ast:letrec #f addr* rhs* body))))
