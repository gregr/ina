#lang racket/base
(provide
  define-record
  define-record-variant
  )
(require
  racket/vector
  )

(define-syntax define-record
  (syntax-rules ()
    ((_ name name?)
     (begin (define name (vector 'name))
            (define (name? datum) (eq? name datum))))
    ((_ name name? f* ...)
     (begin (define (name? datum)
              (and (vector? datum) (eq? 'name (vector-ref datum 0))))
            (define-record-etc name 1 (f* ...) ())))))
(define-syntax define-record-etc
  (syntax-rules ()
    ((_ name _ () (field* ...))
     (define (name field* ...) (vector 'name field* ...)))
    ((_ name index ((get set) f* ...) field*)
     (begin (define-record-etc name index (get f* ...) field*)
            (define (set datum value)
              (let ((new (vector-copy datum)))
                (vector-set! new index value)
                new))))
     ((_ name index (get f* ...) (field* ...))
      (begin (define-record-etc name (+ 1 index) (f* ...) (field* ... get))
             (define (get datum) (vector-ref datum index))))))

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
