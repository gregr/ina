(define (identifier->fresh-address p) (fresh-address (syntax-peek p)))

(define ($provenance   pv E)          (E-provenance-add E pv))
(define ($quote        value)         (E:quote          value))
(define ($ref          addr)          (E:ref            addr))
(define ($call         rator . rand*) (E:call           rator rand*))
(define ($apply/values rator vrand)   (E:apply/values   rator vrand))
(define ($if           c t f)         (E:if             c t f))

(define ($case-lambda param* ^body*)
  (let* ((addr*~* (map (lambda (param) (improper-list-map identifier->fresh-address param)) param*))
         (body*   (map (lambda (addr*~ ^body) (apply ^body (map $ref (improper-list->list addr*~))))
                       addr*~* ^body*)))
    (E:case-lambda addr*~* body*)))

(define ($letrec param* ^rhs*&body)
  (let ((addr* (map identifier->fresh-address param*)))
    (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
      (E:letrec addr* rhs* body))))
