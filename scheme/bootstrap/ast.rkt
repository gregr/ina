#lang racket/base
(provide
  ast?
  ast->v
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

(define-type ast ast? ast->v)

(define (ast-literal datum)    (ast `#(quote ,datum)))
(define (ast-variable address) (ast `#(var ,address)))
(define (ast-set! addr value)  (ast `#(set! ,addr ,value)))
(define (ast-if c t f)         (ast `#(if ,c ,t ,f)))
(define (ast-apply proc args)  (ast `#(apply ,proc ,args)))
(define (ast-apply* proc arg*) (ast `#(apply* ,proc ,arg*)))

(define (ast-lambda variadic? address?* body)
  (ast `#(lambda ,variadic? ,address?* ,body)))

(define (ast-reset body)    (ast `#(reset ,body)))
(define (ast-shift proc)    (ast `#(shift ,proc)))
(define (ast-unshift k arg) (ast `#(unshift ,k ,arg)))

(define (ast-primitive-op name a*) (ast `#(prim-op ,name ,a*)))
