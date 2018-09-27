(provide ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:reset ast:shift ast:prim)

(define (ast:quote datum)     (vector 'quote datum))
(define (ast:var address)     (vector 'var address))
(define (ast:set! addr value) (vector 'set! addr value))
(define (ast:if c t f)        (vector 'if c t f))
(define (ast:apply proc arg)  (vector 'apply proc arg))
(define (ast:reset body)      (vector 'reset body))
(define (ast:shift proc)      (vector 'shift proc))

(define (ast:lambda variadic? address?* body)
  (vector 'lambda variadic? address?* body))

(define (ast:prim name a*) (vector 'prim name a*))
