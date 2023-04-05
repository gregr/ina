(define (error . detail*) (apply panic 'error detail*))

(define (type-error expected value) (error 'type 'expected expected 'given value))

(define (has-type?! type? expected value) (unless (type? value) (type-error expected value)))

(splicing-local
  ((define rtd.error (make-rtd 'error #f #f '#(detail))))
  (define (make-error . detail*) ((record-constructor rtd.error) detail*))
  (define error?        (record-predicate rtd.error))
  (define error-detail* (record-field-position-accessor rtd.error 0)))

(define (make-lexical-error description location)
  (make-error 'lexical (cons 'description description) (cons 'location location)))

(define (make-syntax-error description . stx*)
  (make-error 'syntax (cons 'description description) (cons 'location* stx*)))

(define (make-system-error kind . detail*) (apply make-error (cons 'system kind) detail*))

(define (raise-lexical-error description location)
  (raise (make-lexical-error description location)))

(define (raise-syntax-error description . stx*) (raise (apply make-syntax-error description stx*)))
(define (raise-system-error sub-kind . detail*) (raise (apply make-system-error sub-kind detail*)))

;; TODO: programmable condition handling
(define (raise c) (panic 'raise c))
