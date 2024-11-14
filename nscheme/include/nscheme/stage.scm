(define ($annotate     E note)        (E-annotate E note))
(define ($quote        value)         (E:quote        #f value))
(define ($ref          addr)          (E:ref          #f addr))
(define ($if           c t f)         (E:if           #f c t f))
(define ($apply/values rator vrand)   (E:apply/values #f rator vrand))
(define ($call*        rator rand*)   (E:call         #f rator rand*))
(define ($call         rator . rand*) ($call* rator rand*))

(splicing-local
  ((define (param->address stx)
     (let ((x (syntax-unwrap stx)))
       (unless (or (symbol? x) (not x)) (error "not a formal parameter" stx))
       (make-address x stx))))
  (define ($case-lambda param*~* ^body*)
    (let* ((addr*~* (map (lambda (param) (improper-list-map param->address param)) param*~*))
           (body*   (map (lambda (addr*~ ^body)
                           (apply ^body (map $ref (improper-list->list addr*~))))
                         addr*~* ^body*)))
      (E:case-lambda #f addr*~* body*)))
  (define ($letrec param* ^rhs*&body)
    (let ((addr* (map param->address param*)))
      (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
        (E:letrec #f addr* rhs* body)))))

(define ($lambda param*~     ^body) ($case-lambda (list param*~) (list ^body)))
(define ($let    param* rhs* ^body) ($call* ($lambda param* ^body) rhs*))
(define ($begin  e . e*)
  (let loop ((e e) (e* e*))
    (if (null? e*) e ($apply/values ($lambda #f (lambda (_) (loop (car e*) (cdr e*)))) e))))
