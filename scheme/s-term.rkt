#lang racket/base
(provide
  s-term?
  s-literal s-literal? s-literal-datum
  s-identifier s-identifier? s-identifier-name
  s-primitive-operation s-primitive-operation? s-primitive-operation-op
  s-if s-if? s-if-c s-if-t s-if-f
  s-app s-app? s-app-proc s-app-arg*
  s-lambda s-lambda? s-lambda-param* s-lambda-body
  s-letfix s-letfix? s-letfix-binding* s-letfix-body


  s-syntax s-syntax? s-syntax-body
  s-quasisyntax s-quasisyntax? s-quasisyntax-body
  s-unsyntax s-unsyntax? s-unsyntax-body
  s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body

  s-set! s-set!? s-set!-name s-set!-body
  s-shift s-shift? s-shift-tag s-shift-name s-shift-body
  s-reset s-reset? s-reset-tag s-reset-body
  )
(require "record.rkt")

(define-record-variant
  s-term?
  (s-literal s-literal? s-literal-datum)
  (s-identifier s-identifier? s-identifier-name)
  (s-primitive-operation s-primitive-operation? s-primitive-operation-op)
  (s-if s-if? s-if-c s-if-t s-if-f)
  (s-app s-app? s-app-proc s-app-arg*)
  (s-lambda s-lambda? s-lambda-param* s-lambda-body)
  (s-letfix s-letfix? s-letfix-binding* s-letfix-body)

  (s-syntax s-syntax? s-syntax-body)
  (s-quasisyntax s-quasisyntax? s-quasisyntax-body)
  (s-unsyntax s-unsyntax? s-unsyntax-body)
  (s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body)

  (s-set! s-set!? s-set!-name s-set!-body)
  (s-shift s-shift? s-shift-tag s-shift-name s-shift-body)
  (s-reset s-reset? s-reset-tag s-reset-body)
  )
