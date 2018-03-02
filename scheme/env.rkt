#lang racket/base
(provide
  e-address? e-address-ei e-address-fi
  env-empty
  env-extend
  env-find
  env-lookup
  env-ref
  list->frame
  )
(require "type.rkt")

(define-type e-address e-address? e-address-ei e-address-fi)
(define (list->frame x*) (list->vector x*))
(define env-empty '())
(define (env-extend env f) (cons f env))
(define (env-extend* env x*) (env-extend env (list->frame x*)))
(define (env-ref f* address)
  (vector-ref (list-ref f* (e-address-ei address)) (e-address-fi address)))
(define (env-find f* b?)
  (let loop-frame ((f* f*) (ei 0))
    (and (not (null? f*))
         (let loop-binding ((b* (car f*)) (fi 0))
           (cond ((null? b*) (loop-frame (cdr f*) (+ 1 ei)))
                 ((b? (car b*)) (cons (car b*) (e-address ei fi)))
                 (else (loop-binding (cdr b*) (+ 1 fi))))))))
(define (env-lookup env name)
  (env-find env (lambda (binding) (equal? name (car binding)))))
