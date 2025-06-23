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
    (make-parse-quasi vocab.quasiquote-syntax 'quasiquote-syntax 'unsyntax 'unsyntax-splicing #f
                      parse-expression parse-quote-syntax $cons $list->vector $append #f)
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
            (else (raise-parse-error "not an identifier-syntax form" stx)))))
  (define ((identifier-only-syntax op) stx) (parse-identifier stx) (op stx)))

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
(define-syntax (identifier-only-syntax-rule stx)
  (apply (lambda (_ rhs) (quasiquote-syntax (identifier-only-syntax (lambda (_) (quote-syntax #,rhs)))))
         (syntax->list stx)))
(define-syntax (define-identifier-syntax-rule stx)
  (apply (lambda (_ lhs rhs) (quasiquote-syntax (define-syntax #,lhs (identifier-syntax-rule #,rhs))))
         (syntax->list stx)))

(define-vocabulary-syntax-binder define-set!-syntax define-in-vocabulary vocab.set! #f parse-expression)
(define-vocabulary-syntax-binder add-set!-syntax    add-in-vocabulary    vocab.set! #f parse-expression)

(define-syntax (define-ref&set!-syntax stx)
  (apply (lambda (_ name transform-ref transform-set!)
           (quasiquote-syntax (splicing-local ((define-expression-syntax #,name #,transform-ref))
                                (add-set!-syntax #,name #,transform-set!))))
         (syntax->list stx)))
(define-syntax (define-ref&set!-identifier-syntax stx)
  (apply (lambda (_ name transform-ref transform-set!)
           (quasiquote-syntax (define-ref&set!-syntax #,name
                                (identifier-syntax #,transform-ref)
                                (identifier-only-syntax #,transform-set!))))
         (syntax->list stx)))
(define-syntax (define-ref&set!-identifier-syntax-rule stx)
  (apply (lambda (_ name ref-result set!-result)
           (quasiquote-syntax (define-ref&set!-syntax #,name
                                (identifier-syntax-rule #,ref-result)
                                (identifier-only-syntax-rule #,set!-result))))
         (syntax->list stx)))
(define-syntax (add-set!-identifier-syntax stx)
  (apply (lambda (_ name transform-set!)
           (quasiquote-syntax (add-set!-syntax #,name (identifier-only-syntax #,transform-set!))))
         (syntax->list stx)))
(define-syntax (add-set!-identifier-syntax-rule stx)
  (apply (lambda (_ name transform-set!)
           (quasiquote-syntax (add-set!-syntax #,name (identifier-only-syntax-rule #,transform-set!))))
         (syntax->list stx)))
