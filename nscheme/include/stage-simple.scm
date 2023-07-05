(define (identifier->fresh-address p) (fresh-address (syntax-peek p)))

(define ($provenance  pv E) (E-provenance-add E pv))
(define ($prim        name) (E:prim  #f name))
(define ($quote      value) (E:quote #f value))
(define ($ref         addr) (E:ref   #f addr))
(define ($call proc . args) (E:call  #f proc args))
(define ($if         c t f) (E:if    #f c t f))

(define ($case-lambda . cc*)
  (define ($case-lambda-clause param*~ arg*->body)
    (let* ((addr*~ (improper-list-map identifier->fresh-address param*~)))
      (case-lambda-clause addr*~ (apply arg*->body (map $ref (improper-list->list addr*~))))))
  (E:case-lambda #f (map (lambda (cc) ($case-lambda-clause (car cc) (cdr cc))) cc*)))

(define ($letrec param* ^rhs*&body)
  (let ((addr* (map identifier->fresh-address param*)))
    (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
      (E:letrec #f addr* rhs* body))))
