#lang racket/base
(provide
  s-term?
  s-metadata s-metadata? s-metadata-md s-metadata-s
  s-literal s-literal? s-literal-datum
  s-identifier s-identifier? s-identifier-name
  s-primitive-operation s-primitive-operation? s-primitive-operation-op
  s-if s-if? s-if-c s-if-t s-if-f
  s-app s-app? s-app-proc s-app-arg*
  s-lambda s-lambda? s-lambda-param* s-lambda-body

  s-define s-define? s-define-name s-define-body
  s-let s-let? s-let-binding* s-let-body
  s-let* s-let*? s-let*-binding* s-let*-body
  s-letrec s-letrec? s-letrec-binding* s-letrec-body
  s-letrec* s-letrec*? s-letrec*-binding* s-letrec*-body
  s-do s-do? s-do-binding-step* s-do-test s-do-finally* s-do-normally*

  s-begin s-begin? s-begin-body*
  s-match s-match? s-match-arg s-match-clause*
  s-case s-case? s-case-arg s-case-clause*
  s-cond s-cond? s-cond-clause*
  s-and s-and? s-and-body*
  s-or s-or? s-or-body*
  s-when s-when? s-when-body*
  s-unless s-unless? s-unless-body*

  s-quasiquote s-quasiquote? s-quasiquote-body
  s-quote s-quote? s-quote-body
  s-unquote s-unquote? s-unquote-body
  s-unquote-splicing s-unquote-splicing? s-unquote-splicing-body

  s-quasisyntax s-quasisyntax? s-quasisyntax-body
  s-syntax s-syntax? s-syntax-body
  s-unsyntax s-unsyntax? s-unsyntax-body
  s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body

  s-set! s-set!? s-set!-name s-set!-body
  s-shift s-shift? s-shift-tag s-shift-name s-shift-body
  s-reset s-reset? s-reset-tag s-reset-body
  )
(require "record.rkt")

(define-record-variant
  s-term?
  (s-metadata s-metadata? s-metadata-md s-metadata-s)
  (s-literal s-literal? s-literal-datum)
  (s-identifier s-identifier? s-identifier-name)
  (s-primitive-operation s-primitive-operation? s-primitive-operation-op)
  (s-if s-if? s-if-c s-if-t s-if-f)
  (s-app s-app? s-app-proc s-app-arg*)
  (s-lambda s-lambda? s-lambda-param* s-lambda-body)

  (s-define s-define? s-define-name s-define-body)
  (s-let s-let? s-let-binding* s-let-body)
  (s-let* s-let*? s-let*-binding* s-let*-body)
  (s-letrec s-letrec? s-letrec-binding* s-letrec-body)
  (s-letrec* s-letrec*? s-letrec*-binding* s-letrec*-body)
  (s-do s-do? s-do-binding-step* s-do-test s-do-finally* s-do-normally*)

  (s-begin s-begin? s-begin-body*)
  (s-match s-match? s-match-arg s-match-clause*)
  (s-case s-case? s-case-arg s-case-clause*)
  (s-cond s-cond? s-cond-clause*)
  (s-and s-and? s-and-body*)
  (s-or s-or? s-or-body*)
  (s-when s-when? s-when-body*)
  (s-unless s-unless? s-unless-body*)

  (s-quasiquote s-quasiquote? s-quasiquote-body)
  (s-quote s-quote? s-quote-body)
  (s-unquote s-unquote? s-unquote-body)
  (s-unquote-splicing s-unquote-splicing? s-unquote-splicing-body)

  (s-quasisyntax s-quasisyntax? s-quasisyntax-body)
  (s-syntax s-syntax? s-syntax-body)
  (s-unsyntax s-unsyntax? s-unsyntax-body)
  (s-unsyntax-splicing s-unsyntax-splicing? s-unsyntax-splicing-body)

  (s-set! s-set!? s-set!-name s-set!-body)
  (s-shift s-shift? s-shift-tag s-shift-name s-shift-body)
  (s-reset s-reset? s-reset-tag s-reset-body)
  )
