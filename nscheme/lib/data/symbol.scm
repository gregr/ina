(provide gensym symbol? symbol->string)

(require tagged-vector? box unbox)

(define (gensym name) (vector 'symbol (box name)))
(define (symbol? d)   (tagged-vector? 'symbol '(name) d))
(define (symbol->string sym)
  (unless (symbol? sym) (error '"invalid symbol:" sym))
  (unbox (vector-ref sym 1)))
