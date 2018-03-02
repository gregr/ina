#lang racket/base
(provide
  s-term?
  s-literal s-literal? s-literal-datum
  s-variable s-variable? s-variable-id
  s-lambda s-lambda? s-lambda-id s-lambda-body
  s-lambda* s-lambda*? s-lambda*-min s-lambda*-wrapped
  s-letrec s-letrec? s-letrec-id s-letrec-init* s-letrec-body
  s-apply s-apply? s-apply-proc s-apply-arg*
  s-apply* s-apply*? s-apply*-proc s-apply*-arg
  s-if s-if? s-if-c s-if-t s-if-f
  s-seq s-seq? s-seq-effect* s-seq-result
  s-primitive-operation s-primitive-operation? s-primitive-operation-op

  s-syntax s-syntax? s-syntax-body
  s-quasisyntax s-quasisyntax? s-quasisyntax-body
  s-unsyntax s-unsyntax? s-unsyntax-body
  s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body

  s-set! s-set!? s-set!-id s-set!-body

  s-shift s-shift? s-shift-id s-shift-body
  s-reset s-reset? s-reset-body

  s-id s-id? s-id-base s-id-offset
  )
(require "type.rkt")

(define-variant-type
  s-term?
  (s-literal s-literal? s-literal-datum)
  (s-variable s-variable? s-variable-id)
  (s-lambda s-lambda? s-lambda-id s-lambda-body)
  (s-lambda* s-lambda*? s-lambda*-min s-lambda*-wrapped)
  (s-letrec s-letrec? s-letrec-id s-letrec-init* s-letrec-body)
  (s-apply s-apply? s-apply-proc s-apply-arg*)
  (s-apply* s-apply*? s-apply*-proc s-apply*-arg)
  (s-if s-if? s-if-c s-if-t s-if-f)
  (s-seq s-seq? s-seq-effect* s-seq-result)
  (s-primitive-operation s-primitive-operation? s-primitive-operation-op)

  (s-syntax s-syntax? s-syntax-body)
  (s-quasisyntax s-quasisyntax? s-quasisyntax-body)
  (s-unsyntax s-unsyntax? s-unsyntax-body)
  (s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body)

  (s-set! s-set!? s-set!-id s-set!-body)

  (s-shift s-shift? s-shift-id s-shift-body)
  (s-reset s-reset? s-reset-body)
  )

(define-type s-id s-id? s-id-base s-id-offset)
