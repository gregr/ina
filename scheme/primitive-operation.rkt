#lang racket/base
(provide
  po?
  po-list->vector po-list->vector? po-list->vector-l
  po-vector po-vector? po-vector-x*
  po-vector-length po-vector-length? po-vector-length-v
  po-vector-ref po-vector-ref? po-vector-ref-v

  po-cons po-cons? po-cons-a po-cons-d
  po-car po-car? po-car-c
  po-cdr po-cdr? po-cdr-c

  po-vectorp po-vectorp? po-vectorp-x
  po-pairp po-pairp? po-pairp-x
  po-nullp po-nullp? po-nullp-x
  po-symbolp po-symbolp? po-symbolp-x
  po-numberp po-numberp? po-numberp-x
  po-procedurep po-procedurep? po-procedurep-x

  po-boolean= po-boolean=? po-boolean=-x
  po-symbol= po-symbol=? po-symbol=-x
  po-= po-=? po-=-x

  po-<= po-<=? po-<=-a po-<=-b
  po-< po-<? po-<-a po-<-b
  po-+ po-+? po-+-a po-+-b
  po-* po-*? po-*-a po-*-b

  po-vector-set! po-vector-set!? po-vector-set!-v po-vector-set!-i po-vector-set!-x
  )
(require "record.rkt")

(define-record-variant
  po?
  (po-list->vector po-list->vector? po-list->vector-l)
  (po-vector po-vector? po-vector-x*)
  (po-vector-length po-vector-length? po-vector-length-v)
  (po-vector-ref po-vector-ref? po-vector-ref-v)

  (po-cons po-cons? po-cons-a po-cons-d)
  (po-car po-car? po-car-c)
  (po-cdr po-cdr? po-cdr-c)

  (po-vectorp po-vectorp? po-vectorp-x)
  (po-pairp po-pairp? po-pairp-x)
  (po-nullp po-nullp? po-nullp-x)
  (po-symbolp po-symbolp? po-symbolp-x)
  (po-numberp po-numberp? po-numberp-x)
  (po-procedurep po-procedurep? po-procedurep-x)

  (po-boolean= po-boolean=? po-boolean=-x)
  (po-symbol= po-symbol=? po-symbol=-x)
  (po-= po-=? po-=-x)

  (po-<= po-<=? po-<=-a po-<=-b)
  (po-< po-<? po-<-a po-<-b)
  (po-+ po-+? po-+-a po-+-b)
  (po-* po-*? po-*-a po-*-b)

  (po-vector-set! po-vector-set!? po-vector-set!-v po-vector-set!-i po-vector-set!-x)
  )
