(define (identifier->fresh-address p) (fresh-address (syntax-peek p)))

(define ($provenance  pv E) (E-provenance-add E pv))
(define ($prim        name) (E:prim  name))
(define ($quote      value) (E:quote value))
(define ($ref         addr) (E:ref   addr))
(define ($call proc . args) (E:call  proc args))
(define ($if         c t f) (E:if    c t f))

(define ($case-lambda . cc*)
  (let* ((addr*~* (map (lambda (cc) (improper-list-map identifier->fresh-address (car cc))) cc*))
         (body*   (map (lambda (addr*~ cc) (apply (cdr cc) (map $ref (improper-list->list addr*~))))
                       addr*~* cc*)))
    (E:case-lambda addr*~* body*)))

(define ($letrec param* ^rhs*&body)
  (let ((addr* (map identifier->fresh-address param*)))
    (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
      (E:letrec addr* rhs* body))))
