#lang racket/base
(provide make-mvector mvector? mvector->vector
         mvector-length mvector-ref mvector-set! mvector-cas!
         string->vector vector->string)
(require racket/struct racket/vector)

(struct mvector (v)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (mv) 'mvector)
             (lambda (mv) (vector->list (mvector-v mv)))))))

(define (make-mvector k x)          (mvector (make-vector k x)))
(define (mvector->vector mv)        (vector-copy   (mvector-v mv)))
(define (mvector-length mv)         (vector-length (mvector-v mv)))
(define (mvector-ref mv i)          (vector-ref    (mvector-v mv) i))
(define (mvector-set! mv i x)       (vector-set!   (mvector-v mv) i x))
(define (mvector-cas! mv i old new) (vector-cas!   (mvector-v mv) i old new))

(define (string->vector s) (list->vector (string->list s)))
(define (vector->string v) (list->string (vector->list v)))
