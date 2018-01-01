#lang racket/base
(provide
  s-term?
  s-literal s-literal? s-literal-datum
  s-variable s-variable? s-variable-id
  s-lambda s-lambda? s-lambda-id s-lambda-body
  s-apply s-apply? s-apply-proc s-apply-arg
  s-if s-if? s-if-c s-if-t s-if-f
  s-primitive-operation s-primitive-operation? s-primitive-operation-op

  s-syntax s-syntax? s-syntax-body
  s-quasisyntax s-quasisyntax? s-quasisyntax-body
  s-unsyntax s-unsyntax? s-unsyntax-body
  s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body

  s-shift s-shift? s-shift-body
  s-reset s-reset? s-reset-body
  )
(require "record.rkt")

(define-record-variant
  s-term?
  (s-literal s-literal? s-literal-datum)
  (s-variable s-variable? s-variable-id)
  (s-lambda s-lambda? s-lambda-id s-lambda-body)
  (s-apply s-apply? s-apply-proc s-apply-arg)
  (s-if s-if? s-if-c s-if-t s-if-f)
  (s-primitive-operation s-primitive-operation? s-primitive-operation-op)

  (s-syntax s-syntax? s-syntax-body)
  (s-quasisyntax s-quasisyntax? s-quasisyntax-body)
  (s-unsyntax s-unsyntax? s-unsyntax-body)
  (s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body)

  (s-shift s-shift? s-shift-body)
  (s-reset s-reset? s-reset-body)
  )
