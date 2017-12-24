#lang racket/base
(provide
  po?
  po-vector po-vector? po-vector-x*
  po-vector-length po-vector-length? po-vector-length-v
  po-vector-ref po-vector-ref? po-vector-ref-v

  po-cons po-cons? po-cons-a po-cons-d
  po-car po-car? po-car-c
  po-cdr po-cdr? po-cdr-c

  po-char->integer po-char->integer? po-char->integer-x
  po-integer->char po-integer->char? po-integer->char-x
  po-string->vector po-string->vector? po-string->vector-x
  po-vector->string po-vector->string? po-vector->string-x
  po-string->symbol po-string->symbol? po-string->symbol-x
  po-symbol->string po-symbol->string? po-symbol->string-x
  po-mvector->vector po-mvector->vector? po-mvector->vector-v

  po-charp po-charp? po-charp-x
  po-stringp po-stringp? po-stringp-x
  po-vectorp po-vectorp? po-vectorp-x
  po-mvectorp po-mvectorp? po-mvectorp-x
  po-pairp po-pairp? po-pairp-x
  po-symbolp po-symbolp? po-symbolp-x
  po-numberp po-numberp? po-numberp-x
  po-procedurep po-procedurep? po-procedurep-x

  po-eq po-eq? po-eq-x po-eq-y
  po-eqv po-eqv? po-eqv-x po-eqv-y

  po-= po-=? po-=-x
  po-<= po-<=? po-<=-a po-<=-b
  po-< po-<? po-<-a po-<-b
  po-+ po-+? po-+-a po-+-b
  po-* po-*? po-*-a po-*-b

  po-make-mvector po-make-mvector? po-make-mvector-n
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

  (po-char->integer po-char->integer? po-char->integer-x)
  (po-integer->char po-integer->char? po-integer->char-x)
  (po-string->vector po-string->vector? po-string->vector-x)
  (po-vector->string po-vector->string? po-vector->string-x)
  (po-string->symbol po-string->symbol? po-string->symbol-x)
  (po-symbol->string po-symbol->string? po-symbol->string-x)
  (po-mvector->vector po-mvector->vector? po-mvector->vector-v)

  (po-charp po-charp? po-charp-x)
  (po-stringp po-stringp? po-stringp-x)
  (po-vectorp po-vectorp? po-vectorp-x)
  (po-mvectorp po-mvectorp? po-mvectorp-x)
  (po-pairp po-pairp? po-pairp-x)
  (po-symbolp po-symbolp? po-symbolp-x)
  (po-numberp po-numberp? po-numberp-x)
  (po-procedurep po-procedurep? po-procedurep-x)

  (po-eq po-eq? po-eq-x po-eq-y)
  (po-eqv po-eqv? po-eqv-x po-eqv-y)

  (po-= po-=? po-=-x)
  (po-<= po-<=? po-<=-a po-<=-b)
  (po-< po-<? po-<-a po-<-b)
  (po-+ po-+? po-+-a po-+-b)
  (po-* po-*? po-*-a po-*-b)

  (po-make-mvector po-make-mvector? po-make-mvector-n)
  (po-mvector-length po-mvector-length? po-mvector-length-v)
  (po-mvector-ref po-mvector-ref? po-mvector-ref-v)
  (po-mvector-set!
    po-mvector-set!? po-mvector-set!-v po-mvector-set!-i po-mvector-set!-x)
  )
