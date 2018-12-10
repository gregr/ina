#lang racket/base
(provide
  procedure? mvector? vector? pair? null? boolean? string?
  number? integer? fixnum? flonum?
  boolean=? number=? string=? mvector=? procedure=?
  string->vector vector->string
  cons car cdr vector-ref vector-length
  make-mvector mvector->vector mvector-set! mvector-ref mvector-length
  ;; TODO: mvector-cas!
  string<? string>?
  = <= < >= > + * - / truncate
  bitwise-and bitwise-ior bitwise-xor bitwise-not
  bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length)

(require racket/bool racket/vector)

(struct mvector (v) #:transparent)
(define (make-mvector k d)          (mvector (make-vector k d)))
(define (mvector=? m n)             (eq? m n))
(define (mvector-length mv)         (vector-length (mvector-v mv)))
(define (mvector-ref mv i)          (vector-ref (mvector-v mv) i))
(define (mvector-set! mv i new)     (vector-set! (mvector-v mv) i new) #t)
;; TODO: update Racket to use this.
;(define (mvector-cas! mv i old new) (vector-cas! (mvector-v mv) i old new))
(define (mvector->vector mv)        (vector-copy (mvector-v mv)))
(define (string->vector s)
  (list->vector (map char->integer (string->list s))))
(define (vector->string v)
  (list->string (map integer->char (vector->list v))))
(define (procedure=? m n) (eq? m n))
(define (number=? m n)    (eqv? m n))
