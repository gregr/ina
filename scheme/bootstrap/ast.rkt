#lang racket/base
(provide
  ast-literal
  ast-variable
  ast-set!
  ast-if
  ast-apply
  ast-apply*
  ast-lambda
  ast-reset
  ast-shift
  ast-unshift
  ast-primitive-op
  )

(require "../type.rkt")

(define (ast-literal datum)    `#(quote ,datum))
(define (ast-variable address) `#(var ,address))
(define (ast-set! addr value)  `#(set! ,addr ,value))
(define (ast-if c t f)         `#(if ,c ,t ,f))
(define (ast-apply proc args)  `#(apply ,proc ,args))
(define (ast-apply* proc arg*) `#(apply* ,proc ,arg*))

(define (ast-lambda variadic? address?* body)
  `#(lambda ,variadic? ,address?* ,body))

(define (ast-reset body)    `#(reset ,body))
(define (ast-shift proc)    `#(shift ,proc))
(define (ast-unshift k arg) `#(unshift ,k ,arg))

(define (ast-primitive-op name a*) `#(prim-op ,name ,a*))
