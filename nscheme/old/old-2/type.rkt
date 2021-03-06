#lang racket/base
(provide
  define-type
  define-type*
  define-vector-type
  )
(require
  racket/struct
  racket/vector
  )

(define tag=>details (hash))
(define tag-current -1)
(define (tag-fresh! name repr)
  (set! tag-current (+ 1 tag-current))
  (set! tag=>details (hash-set tag=>details tag-current (cons name repr)))
  tag-current)
(define (tag->details tag) (hash-ref tag=>details tag))

(struct record (tag payload)
        #:transparent
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (d) (car (tag->details (record-tag d))))
             (lambda (d)
               (define repr (cdr (tag->details (record-tag d))))
               (if repr (repr d) (vector->list (record-payload d))))))))

(define-syntax define-type
  (syntax-rules ()
    ((_ (name singleton-name repr) name?)
     (define-type/singleton name singleton-name repr name?))
    ((_ (name construct repr) name? f* ...)
     (define-type/multi-field name construct repr name? f* ...))
    ((_ (name c) name? f* ...) (define-type (name c #f) name? f* ...))
    ((_ name name? f* ...) (define-type (name name #f) name? f* ...))))
(define-syntax define-type/singleton
  (syntax-rules ()
    ((_ name singleton-name repr name?)
     (begin (define tag (tag-fresh! 'name repr))
            (define singleton-name (record tag '#()))
            (define (name? datum) (eq? singleton-name datum))))))
(define-syntax define-type/multi-field
  (syntax-rules ()
    ((_ name construct repr name? f* ...)
     (begin (define tag (tag-fresh! 'name repr))
            (define (name? datum)
              (and (record? datum) (eq? tag (record-tag datum))))
            (define-type-etc name? tag construct 0 (f* ...) ())))))
(define-syntax define-type-etc
  (syntax-rules ()
    ((_ name? tag construct _ () (field* ...))
     (define (construct field* ...) (record tag (vector field* ...))))
    ((_ name? tag construct index ((get set) f* ...) field*)
     (begin (define-type-etc name? tag construct index (get f* ...) field*)
            (define (set datum value)
              (let ((new (vector-copy (record-payload datum))))
                (vector-set! new index value)
                (record tag new)))))
    ((_ name? tag construct index (get f* ...) (field* ...))
     (begin (define-type-etc name? tag construct (+ 1 index) (f* ...) (field* ... get))
            (define (get datum)
              (if (name? datum)
                (vector-ref (record-payload datum) index)
                (error (format "~a with wrong argument type:" 'get)
                       datum)))))))

(define-syntax define-type*
  (syntax-rules ()
    ((_ vp? v* ...) (define-type*-etc vp? (v* ...) ()))))
(define-syntax define-type*-etc
  (syntax-rules ()
    ((_ vp? () (p?* ...))
     (define (vp? d) (ormap (lambda (p?) (p? d)) (list p?* ...))))
    ((_ vp? ((vname v? field* ...) v* ...) (p?* ...))
     (begin (define-type vname v? field* ...)
            (define-type*-etc vp? (v* ...) (p?* ... v?))))))

(define-syntax define-vector-type
  (syntax-rules ()
    ((_ (name singleton-name) name?)
     (define-vector-type/singleton name singleton-name name?))
    ((_ (name construct) name? f* ...)
     (define-vector-type/multi-field name construct name? f* ...))
    ((_ name name? f* ...) (define-vector-type (name name) name? f* ...))))
(define-syntax define-vector-type/singleton
  (syntax-rules ()
    ((_ name singleton-name name?)
     (begin (define singleton-name '#(name))
            (define (name? datum) (eq? singleton-name datum))))))
(define-syntax define-vector-type/multi-field
  (syntax-rules ()
    ((_ name construct name? f* ...)
     (begin (define (name? datum) (and (vector? datum)
                                       (<= 1 (vector-length datum))
                                       (eq? 'name (vector-ref datum 0))))
            (define-vector-type-etc name? name construct 1 (f* ...) ())))))
(define-syntax define-vector-type-etc
  (syntax-rules ()
    ((_ name? tag construct _ () (field* ...))
     (define (construct field* ...) (vector 'tag field* ...)))
    ((_ name? tag construct index ((get set) f* ...) field*)
     (begin (define-vector-type-etc
              name? tag construct index (get f* ...) field*)
            (define (set datum value) (let ((new (vector-copy datum)))
                                        (vector-set! new index value)
                                        new))))
    ((_ name? tag construct index (get f* ...) (field* ...))
     (begin (define-vector-type-etc
              name? tag construct (+ 1 index) (f* ...) (field* ... get))
            (define (get datum)
              (if (name? datum)
                (vector-ref datum index)
                (error (format "~a with wrong argument type:" 'get)
                       datum)))))))
