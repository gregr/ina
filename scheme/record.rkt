#lang racket/base
(provide
  define-record
  define-record-variant
  record?
  record-payload
  record-tag
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

(define-syntax define-record
  (syntax-rules ()
    ((_ name name?)
     (begin (define name (record 'name '#()))
            (define (name? datum) (eq? name datum))))
    ((_ name name? f* ...)
     (begin (define (name? datum)
              (and (record? datum) (eq? 'name (record-tag datum))))
            (define-record-etc name 0 (f* ...) ())))))
(define-syntax define-record-etc
  (syntax-rules ()
    ((_ name _ () (field* ...))
     (define (name field* ...) (record 'name (vector field* ...))))
    ((_ name index ((get set) f* ...) field*)
     (begin (define-record-etc name index (get f* ...) field*)
            (define (set datum value)
              (let ((new (vector-copy (record-payload datum))))
                (vector-set! new index value)
                (record 'name new)))))
     ((_ name index (get f* ...) (field* ...))
      (begin (define-record-etc name (+ 1 index) (f* ...) (field* ... get))
             (define (get datum) (vector-ref (record-payload datum) index))))))

(define-syntax define-record-variant
  (syntax-rules ()
    ((_ vp? v* ...) (define-record-variant-etc vp? (v* ...) ()))))
(define-syntax define-record-variant-etc
  (syntax-rules ()
    ((_ vp? () (p?* ...))
     (define (vp? d) (ormap (lambda (p?) (p? d)) (list p?* ...))))
    ((_ vp? ((vname v? field* ...) v* ...) (p?* ...))
     (begin (define-record vname v? field* ...)
            (define-record-variant-etc vp? (v* ...) (p?* ... v?))))))
