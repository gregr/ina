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
  (define ((make-macro-parser introduce-definitions? parse) env.op op)
    (let ((transcribe (syntax-transcribe/parse introduce-definitions? parse)))
      (lambda (env.use stx) (transcribe stx op env.op env.use))))
  (define (dismantle-operator-binding stx)
    (apply (lambda (_ lhs . rhs*)
             (unless (pair? rhs*)
               (raise-parse-error "missing right-hand-side expression" stx))
             (when (and (identifier? lhs) (not (null? (cdr rhs*))))
               (raise-parse-error "right-hand-side is not a single expression" stx))
             (let loop ((lhs lhs) (rhs* rhs*))
               (if (identifier? lhs)
                   (values lhs (car rhs*))
                   (let ((lhs*~ (syntax->improper-list lhs)))
                     (unless (pair? lhs*~) (raise-parse-error "not a definable form" lhs))
                     (let ((lhs (car lhs*~)) (param (cdr lhs*~)))
                       (loop lhs (list (quasiquote-syntax (lambda #,param . #,rhs*)))))))))
           (syntax->list stx)))
  (define ((identifier-syntax op) stx)
    (let ((x (syntax-unwrap stx)))
      (cond ((symbol? x) (op stx))
            ((and (pair? x) (identifier? (car x))) (cons (op (car x)) (cdr x)))
            (else (raise-parse-error "not an identifier-syntax form" stx))))))

(define-syntax (define-vocabulary-syntax-binder stx)
  (apply (lambda (_ name bind-in-vocabulary vocabulary-name introduce-definitions? vocabulary-parser)
           (quasiquote-syntax
             (define-syntax (#,name stx)
               (let-values (((lhs rhs) (dismantle-operator-binding stx)))
                 (quasiquote-syntax
                   (#,(quote-syntax #,bind-in-vocabulary) #,lhs
                    #,(quote-syntax #,vocabulary-name)
                    ((make-macro-parser #,(quote-syntax #,introduce-definitions?)
                                        #,(quote-syntax #,vocabulary-parser))
                     (current-environment) #,rhs)))))))
         (syntax->list stx)))

(define-vocabulary-syntax-binder define-definition-syntax define-in-vocabulary vocab.definition #t parse-definition)
(define-vocabulary-syntax-binder define-expression-syntax define-in-vocabulary vocab.expression #f parse-expression)

(define-syntax (define-identifier-syntax stx)
  (let-values (((lhs rhs) (dismantle-operator-binding stx)))
    (quasiquote-syntax (define-syntax #,lhs (identifier-syntax #,rhs)))))

(define-syntax (identifier-syntax-rule stx)
  (apply (lambda (_ rhs) (quasiquote-syntax (identifier-syntax (lambda (_) (quote-syntax #,rhs)))))
         (syntax->list stx)))

(define-syntax (define-identifier-syntax-rule stx)
  (apply (lambda (_ lhs rhs)
           (quasiquote-syntax (define-syntax #,lhs (identifier-syntax-rule #,rhs))))
         (syntax->list stx)))
