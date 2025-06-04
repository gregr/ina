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

(begin-meta
  (define ((macro-parser/parse introduce-definitions? parse) env.op op)
    (let ((transcribe (syntax-transcribe/parse introduce-definitions? parse)))
      (lambda (env.use stx) (transcribe stx op env.op env.use)))))

(define-syntax (define-vocabulary-syntax-binder stx)
  (match (syntax->list stx)
    ((list _ name bind-in-vocabulary vocabulary-name introduce-definitions? vocabulary-parser)
     (quasiquote-syntax
       (define-syntax (#,name stx)
         (match (syntax->list stx)
           ((cons* _ lhs rhs*)
            (let loop ((lhs lhs) (rhs* rhs*))
              (if (identifier? lhs)
                  (begin
                    (unless (null? (cdr rhs*)) (raise-parse-error "too many right-hand-side expressions" stx))
                    (quasiquote-syntax
                      (#,(quote-syntax #,bind-in-vocabulary) #,lhs
                       #,(quote-syntax #,vocabulary-name)
                       ((macro-parser/parse #,(quote-syntax #,introduce-definitions?)
                                            #,(quote-syntax #,vocabulary-parser))
                        (current-environment) #,(car rhs*)))))
                  (match (syntax->list lhs)
                    ((cons lhs param) (loop lhs (list (quasiquote-syntax (lambda #,param . #,rhs*)))))))))))))))

(define-vocabulary-syntax-binder define-definition-syntax define-in-vocabulary vocab.definition #t parse-definition)
(define-vocabulary-syntax-binder define-expression-syntax define-in-vocabulary vocab.expression #f parse-expression)
