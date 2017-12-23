#lang racket/base
(provide
  po?
  po-vector po-vector? po-vector-x*
  po-vector-length po-vector-length? po-vector-length-v
  po-vector-ref po-vector-ref? po-vector-ref-v

  po-cons po-cons? po-cons-a po-cons-d
  po-car po-car? po-car-c
  po-cdr po-cdr? po-cdr-c

  po-vectorp po-vectorp? po-vectorp-x
  po-mvectorp po-mvectorp? po-mvectorp-x
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

  po-make-mvector po-make-mvector? po-make-mvector-n
  po-mvector->vector po-mvector->vector? po-mvector->vector-v
  po-mvector-length po-mvector-length? po-mvector-length-v
  po-mvector-ref po-mvector-ref? po-mvector-ref-v

  po-mvector-set! po-mvector-set!?
  po-mvector-set!-v po-mvector-set!-i po-mvector-set!-x
  )
(require "record.rkt")

(define-record-variant
  po?
  (po-vector po-vector? po-vector-x*)
  (po-vector-length po-vector-length? po-vector-length-v)
  (po-vector-ref po-vector-ref? po-vector-ref-v)

  (po-cons po-cons? po-cons-a po-cons-d)
  (po-car po-car? po-car-c)
  (po-cdr po-cdr? po-cdr-c)

  (po-vectorp po-vectorp? po-vectorp-x)
  (po-mvectorp po-mvectorp? po-mvectorp-x)
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

  (po-make-mvector po-make-mvector? po-make-mvector-n)
  (po-mvector->vector po-mvector->vector? po-mvector->vector-v)
  (po-mvector-length po-mvector-length? po-mvector-length-v)
  (po-mvector-ref po-mvector-ref? po-mvector-ref-v)
  (po-mvector-set!
    po-mvector-set!? po-mvector-set!-v po-mvector-set!-i po-mvector-set!-x)
  )
