(provide
  ast:quote
  ast:var
  ast:set!
  ast:if
  ast:apply
  ast:lambda
  ast:reset
  ast:shift
  ast:error
  ast:primitive-op)

(define (ast:quote datum)     `#(quote ,datum))
(define (ast:var address)     `#(var ,address))
(define (ast:set! addr value) `#(set! ,addr ,value))
(define (ast:if c t f)        `#(if ,c ,t ,f))
(define (ast:apply proc arg)  `#(apply ,proc ,arg))
(define (ast:reset body)      `#(reset ,body))
(define (ast:shift proc)      `#(shift ,proc))
(define (ast:error a)         `#(error ,a))

(define (ast:lambda variadic? address?* body)
  `#(lambda ,variadic? ,address?* ,body))

(define (ast:primitive-op name a*) `#(prim-op ,name ,a*))
