#lang racket/base
(provide
  define-type
  define-variant-type
  )
(require
  racket/struct
  racket/vector
  )

(struct record (tag payload)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (d) (record-tag d))
             (lambda (d) (vector->list (record-payload d)))))))

(define-syntax define-type
  (syntax-rules ()
    ((_ (name singleton-name) name?)
     (define-type/singleton name singleton-name name?))
    ((_ (name construct) name? f* ...)
     (define-type/multi-field name construct name? f* ...))
    ((_ name name?) (define-type/singleton name name name?))
    ((_ name name? f* ...) (define-type/multi-field name name name? f* ...))))
(define-syntax define-type/singleton
  (syntax-rules ()
    ((_ tag singleton-name name?)
     (begin (define singleton-name (record 'tag '#()))
            (define (name? datum) (eq? tag datum))))))
(define-syntax define-type/multi-field
  (syntax-rules ()
    ((_ tag construct name? f* ...)
     (begin (define (name? datum)
              (and (record? datum) (eq? 'tag (record-tag datum))))
            (define-type-etc tag construct 0 (f* ...) ())))))
(define-syntax define-type-etc
  (syntax-rules ()
    ((_ tag construct _ () (field* ...))
     (define (construct field* ...) (record 'tag (vector field* ...))))
    ((_ tag construct index ((get set) f* ...) field*)
     (begin (define-type-etc tag construct index (get f* ...) field*)
            (define (set datum value)
              (let ((new (vector-copy (record-payload datum))))
                (vector-set! new index value)
                (record 'tag new)))))
    ((_ tag construct index (get f* ...) (field* ...))
     (begin (define-type-etc tag construct (+ 1 index) (f* ...) (field* ... get))
            (define (get datum) (vector-ref (record-payload datum) index))))))

(define-syntax define-variant-type
  (syntax-rules ()
    ((_ vp? v* ...) (define-variant-type-etc vp? (v* ...) ()))))
(define-syntax define-variant-type-etc
  (syntax-rules ()
    ((_ vp? () (p?* ...))
     (define (vp? d) (ormap (lambda (p?) (p? d)) (list p?* ...))))
    ((_ vp? ((vname v? field* ...) v* ...) (p?* ...))
     (begin (define-type vname v? field* ...)
            (define-variant-type-etc vp? (v* ...) (p?* ... v?))))))
