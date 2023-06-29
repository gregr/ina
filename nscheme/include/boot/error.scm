(define (error . detail*) (apply panic 'error detail*))

(define (type-error expected value) (error 'type 'expected expected 'given value))

(define (has-type?! type? expected value) (unless (type? value) (type-error expected value)))

(define (make-error . detail*) (vector 'error detail*))
(define (error? x) (and (vector? x) (= (vector-length x) 2) (eq? (vector-ref x 0) 'error)))
(define (error-detail* x) (vector-ref x 1))

;; TODO: move these to where they are used.  And system-error should be more specific.
;(define (make-lexical-error description location)
;  (make-error 'lexical (cons 'description description) (cons 'location location)))
;(define (make-system-error kind . detail*) (apply make-error (cons 'system kind) detail*))
;(define (raise-lexical-error description location)
;  (raise (make-lexical-error description location)))
;(define (raise-system-error sub-kind . detail*) (raise (apply make-system-error sub-kind detail*)))

;; TODO: programmable condition handling
(define (raise c) (panic 'raise c))
