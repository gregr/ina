#lang racket/base
(provide
  datum/provenance
  datum/provenance?
  datum/provenance-provenance
  datum/provenance-datum
  datum/provenance->datum
  datum->datum/provenance
  datum-fold
  )
(require
  "record.rkt"
  racket/vector
  )

(define-record datum/provenance datum/provenance?
               datum/provenance-provenance datum/provenance-datum)

(define (datum-fold pf vf af x->datum x)
  (define datum (x->datum x))
  (define (self x) (datum-fold pf vf af x->datum x))
  (cond ((pair? datum) (pf (cons (self (car datum)) (self (cdr datum)))))
        ((vector? datum) (vf (vector-map self datum)))
        (else (af datum))))

(define (datum/provenance->datum dp)
  (define (id x) x)
  (datum-fold id id id datum/provenance-datum dp))

(define (datum->datum/provenance datum->provenance datum)
  (define (wrap d) (datum/provenance (datum->provenance d) d))
  (datum-fold wrap wrap wrap (lambda (x) x) datum))
