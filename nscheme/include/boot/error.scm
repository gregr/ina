(define (violation kind-or-message . detail*) (apply panic 'violation kind-or-message detail*))

(define (assert-violation location) (violation 'assert location))

(define (type-violation expected value)
  (violation 'type (cons 'expected expected) (cons 'given value)))

(define (procedure-arity-violation proc given)
  (violation 'procedure-arity (cons 'object proc) (cons 'given given)))

(define (index-bounds-violation object index)
  (violation 'index-bounds (cons 'object object) (cons 'given index)))

;; TODO: example problems:
;; - divide-by-zero
;; - overflow
;; - underflow
(define (arithmetic-violation problem operation . operand*)
  (violation 'arithmetic
             (cons 'problem problem) (cons 'operation operation) (cons 'operand* operand*)))

(define (has-type?! type? expected value) (unless (type? value) (type-violation expected value)))

(splicing-local
  ((define rtd.error (vector 2)))
  (define (make-error kind details) (record rtd.error kind details))
  (define (error? x) (and (record? x) (eq? (record-type-descriptor x) rtd.error))))

(define (error?!       x) (has-type?! error? 'error x))
(define (error-kind    e) (error?! e) (record-ref e 0))
(define (error-details e) (error?! e) (record-ref e 1))

(define (make-error:lexical description location)
  (make-error 'lexical (list (cons 'description description)
                             (cons 'location    location))))

(define (make-error:syntax description . stx*)
  (make-error 'syntax (list (cons 'description description)
                            (cons 'location*   stx*))))

;; TODO: example problems:
;; - network
;; - filesystem
;; - input
;; - output
(define (make-error:system problem operation . operand*)
  (make-error 'system (list (cons 'problem   problem)
                            (cons 'operation operation)
                            (cons 'operand*  operand*))))

(define (error . detail*) (raise (make-error #f detail*)))

(define (raise-lexical-error description location)
  (raise (make-error:lexical description location)))

(define (raise-syntax-error description . stx*)
  (raise (apply make-error:syntax description stx*)))

;; TODO: programmable condition handling
(define (raise c)
  (if (error? c)
      (apply panic 'error (error-kind c) (error-details c))
      (panic 'unknown-condition c)))
