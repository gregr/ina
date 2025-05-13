(define-vocabulary current-environment
  vocab.expression
  (operator-parser (lambda (env) ($quote env)) 0 0))
