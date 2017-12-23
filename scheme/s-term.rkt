#lang racket/base
(provide
  s-term?
  s-literal s-literal? s-literal-datum
  s-variable s-variable? s-variable-name
  s-primitive-operation s-primitive-operation? s-primitive-operation-op
  s-if s-if? s-if-c s-if-t s-if-f
  s-app s-app? s-app-proc s-app-arg*
  s-lambda s-lambda? s-lambda-body
  s-letfix s-letfix? s-letfix-binding* s-letfix-body

  s-syntax s-syntax? s-syntax-body
  s-quasisyntax s-quasisyntax? s-quasisyntax-body
  s-unsyntax s-unsyntax? s-unsyntax-body
  s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body

  s-set! s-set!? s-set!-name s-set!-body
  s-shift s-shift? s-shift-body
  s-reset s-reset? s-reset-body

  name?
  name-bound name-bound? name-bound-address
  name-free name-free? name-free-address
  )
(require "record.rkt")

(define-record-variant
  s-term?
  (s-literal s-literal? s-literal-datum)
  (s-variable s-variable? s-variable-name)
  (s-primitive-operation s-primitive-operation? s-primitive-operation-op)
  (s-if s-if? s-if-c s-if-t s-if-f)
  (s-app s-app? s-app-proc s-app-arg*)
  (s-lambda s-lambda? s-lambda-body)
  (s-letfix s-letfix? s-letfix-binding* s-letfix-body)

  (s-syntax s-syntax? s-syntax-body)
  (s-quasisyntax s-quasisyntax? s-quasisyntax-body)
  (s-unsyntax s-unsyntax? s-unsyntax-body)
  (s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body)

  (s-set! s-set!? s-set!-name s-set!-body)
  (s-shift s-shift? s-shift-body)
  (s-reset s-reset? s-reset-body)
  )

(define-record-variant
  name?
  (name-bound name-bound? name-bound-address)
  (name-free name-free? name-free-address)
  )
