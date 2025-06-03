(define-in-vocabulary current-environment
  vocab.expression
  (operator-parser (lambda (env) ($quote env)) 0 0))

(begin-meta
  (define vocab.quasiquote-syntax 'quasiquote-syntax)
  (define (parse-quote-syntax env stx) ($quote (syntax-prune-level stx (current-mark-level)))))

(define-in-vocabulary quote-syntax
  vocab.expression
  (operator-parser parse-quote-syntax 1 1))

(define-in-vocabulary unsyntax          vocab.quasiquote-syntax 'unsyntax)
(define-in-vocabulary unsyntax-splicing vocab.quasiquote-syntax 'unsyntax-splicing)
(define-in-vocabulary quasiquote-syntax
  vocab.quasiquote-syntax 'quasiquote-syntax
  vocab.expression
  (operator-parser
    (parse-quasiquote-X vocab.quasiquote-syntax 'quasiquote-syntax 'unsyntax 'unsyntax-splicing
                        parse-quote-syntax)
    1 1))
