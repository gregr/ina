(provide
  ast-quote
  ast-quote?
  ast-var
  ast-var?
  ast-set!
  ast-set!?
  ast-if
  ast-if?
  ast-apply
  ast-apply?
  ast-apply*
  ast-apply*?
  ast-lambda
  ast-lambda?
  ast-reset
  ast-reset?
  ast-shift
  ast-shift?
  ast-error
  ast-error?
  ast-primitive-op
  ast-primitive-op?)

(require tagged-vector?)

(define (ast-quote datum)      `#(quote ,datum))
(define (ast-quote? a)         (tagged-vector? 'quote '(datum) a))
(define (ast-var address)      `#(var ,address))
(define (ast-var? a)           (tagged-vector? 'var '(address) a))
(define (ast-set! addr value)  `#(set! ,addr ,value))
(define (ast-set!? a)          (tagged-vector? 'set! '(address value) a))
(define (ast-if c t f)         `#(if ,c ,t ,f))
(define (ast-if? a)            (tagged-vector? 'if '(c t f) a))
(define (ast-apply proc args)  `#(apply ,proc ,args))
(define (ast-apply? a)         (tagged-vector? 'apply '(proc args) a))
(define (ast-apply* proc arg*) `#(apply* ,proc ,arg*))
(define (ast-apply*? a)        (tagged-vector? 'apply* '(proc arg*) a))

(define (ast-lambda variadic? address?* body)
  `#(lambda ,variadic? ,address?* ,body))
(define (ast-lambda? a) (tagged-vector? 'lambda '(variadic? a* body) a))

(define (ast-reset body) `#(reset ,body))
(define (ast-reset? a)   (tagged-vector? 'reset '(body) a))
(define (ast-shift proc) `#(shift ,proc))
(define (ast-shift? a)   (tagged-vector? 'shift '(proc) a))
(define (ast-error a*)   `#(error ,a*))
(define (ast-error? a)   (tagged-vector? 'error '(a*) a))

(define (ast-primitive-op name a*) `#(prim-op ,name ,a*))
(define (ast-primitive-op? a)      (tagged-vector? 'prim-op '(name a*) a))
