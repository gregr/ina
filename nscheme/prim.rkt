#lang racket/base
(provide
  param-map param-bind param-apply
  procedure? mvector? vector? pair? null? boolean? string?
  number? integer? fixnum? flonum?
  boolean=? number=? string=? mvector=? procedure=?
  string->vector vector->string cons car cdr vector-ref vector-length
  make-mvector mvector->vector mvector-set! mvector-ref mvector-length
  ;; TODO: mvector-cas!
  string<? string>?
  = <= < >= > + * - / truncate
  bitwise-and bitwise-ior bitwise-xor bitwise-not
  bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length
  (rename-out (nscm-shift shift) (nscm-reset reset)))

(require racket/bool racket/control racket/vector)

(define (lift racket-proc)   (lambda (a) (apply racket-proc a)))
(define (lower nscheme-proc) (lambda a   (nscheme-proc a)))

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

(define (nscm-shift proc) (shift k ((lower proc) (lift k))))
(define (nscm-reset proc) (reset   ((lower proc))))

(define (name->sym n) (string->symbol (if (mvector? n) (mvector-ref n 0) n)))
(define (param-map f p)
  (cond ((pair? p)   (cons (param-map f (car p)) (param-map f (cdr p))))
        ((vector? p) (list->vector (param-map f (vector->list p))))
        ((null? p)   '())
        ((not p)     #f)
        (#t          (f p))))
(define (param-bind param arg)
  (let loop ((p param) (a arg) (bs '()))
    (cond ((and (pair? p) (pair? a))
           (loop (car p) (car a) (loop (cdr p) (cdr a) bs)))
          ((and (vector? p) (vector? a))
           (loop (vector->list p) (vector->list a) bs))
          ((and (null? p) (null? a))                  bs)
          ((not p)                                    bs)
          ((not (or (pair? p) (vector? p) (null? p))) (cons (cons p a) bs))
          (#t (error '"parameter/argument mismatch:"
                     (param-map name->sym param) arg
                     (param-map name->sym p) a)))))
(define (param-apply param arg k) (apply k (map cdr (param-bind param arg))))
