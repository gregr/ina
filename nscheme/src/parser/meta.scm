(define vocab.quasiquote-syntax 'quasiquote-syntax)

;; TODO:
;; - match
;;   - match-define match-lambda match-lambda* match-lambda** match-let match-let*
;;     match-letrec match-letrec*?
;;   - Maybe: syntax-match and/or syntax-rewrite
;; - Maybe: syntax quasisyntax syntax-dismantle syntax-case

;(define vocab.syntax-pattern  'syntax-pattern)  ; e.g., literals and the wildcard _
;(define vocab.syntax-template 'syntax-template) ; e.g., bound template variables
;; TODO: unsyntax and unsyntax-splicing as quasisyntax-template auxiliaries?

;; Other vocabulary ideas
;(define vocab.module  'module)
;(define vocab.grammar 'grammar)
;(define vocab.formula 'formula)
;(define vocab.term    'term)

(define (with-higher-mark-level thunk) (current-mark-level (+ (current-mark-level) 1) thunk))

(define (parse-quote-syntax env stx) ($quote (syntax-prune-level stx (current-mark-level))))

(define parse-quasiquote-syntax
  (parse-quasiquote-X vocab.quasiquote-syntax 'quasiquote-syntax 'unsyntax 'unsyntax-splicing
                      parse-quote-syntax))

(define (parse-begin-meta-definition* env.d env stx*)
  (let ((E (apply/values $quote-values
                         (with-higher-mark-level
                           (lambda ()
                             (E-eval (D->E/publish (parse-begin-definition* env.d env stx*))))))))
    ($d:expression (lambda () E))))
(define (parse-begin-meta-definition env.d env . stx*)
  (parse-begin-meta-definition* env.d env stx*))

(define (parse-begin-meta-expression* env stx*)
  (apply/values $quote-values
                (with-higher-mark-level
                  (lambda ()
                    (E-eval ((expression-operator-parser parse-begin-expression 1 #f) env stx*))))))
(define (parse-begin-meta-expression env . stx*) (parse-begin-meta-expression* env stx*))

(define (parse-current-environment env) ($quote env))

(define ((parse-modify-vocabulary! plist? env-vocabulary-modify!*) env.d env id.lhs . stx*)
  (parse-identifier id.lhs)
  (unless (or (not plist?) (even? (length stx*)))
    (raise-parse-error "not a list of alternating vocabularies and values" stx*))
  (env-vocabulary-modify!*
    env.d env id.lhs (with-higher-mark-level (lambda () (map E-eval (parse-expression* env stx*)))))
  ($d:begin))
(define parse-define-vocabulary  (parse-modify-vocabulary! #t (lambda (env.dst env.src id vx*)
                                                                (env-vocabulary-introduce!* env.dst id vx*))))
(define parse-set-vocabulary!    (parse-modify-vocabulary! #t env-vocabulary-set!*))
(define parse-add-vocabulary!    (parse-modify-vocabulary! #t env-vocabulary-add!*))
(define parse-update-vocabulary! (parse-modify-vocabulary! #t env-vocabulary-update!*))
(define parse-remove-vocabulary! (parse-modify-vocabulary! #f env-vocabulary-remove!*))

(define (parse-define-syntax env.d env.op stx.lhs . stx*.rhs)
  (define (finish id.lhs ^rhs)
    (parse-undefined-identifier env.d id.lhs)
    (let ((op (with-higher-mark-level (lambda () (E-eval (^rhs env.op))))))
      (env-vocabulary-bind!
        env.d id.lhs
        vocab.expression-operator
        (lambda (env.use stx) ((syntax-transcribe/parse parse-expression) stx op env.op env.use))
        vocab.definition-operator
        (lambda (env.d.use env.use stx)
          ((syntax-transcribe/parse-definition parse-definition) stx op env.op env.use env.d.use))))
    ($d:begin))
  (parse-operator-binding finish stx.lhs stx*.rhs))

;; Would syntax-dismantle be helpful enough to justify implementing it?
;(define-syntax syntax-dismantle
;  (syntax-rules ()
;    ((_ x skip clause ...)
;     (let* ((y x) (ex (syntax-unwrap y)))
;       (syntax-dismantle-clauses y ex skip clause ...)))))
;
;(define-syntax syntax-dismantle-clauses
;  (syntax-rules ()
;    ((_ x ex skip) (raise-parse-error "no matching clause" x))
;    ((_ x ex skip (pattern body ...) clause ...)
;     (let ((skip (lambda () (syntax-dismantle-clauses x ex skip clause ...))))
;       (syntax-dismantle-clause x ex skip pattern body ...)))))
;
;(define-syntax (syntax-dismantle-clause stx)
;  (syntax-case stx ()
;    ((_ x ex skip (p.a . p.d) body ...) #'(if (pair? ex)
;                                              (let ((a (car ex))
;                                                    (d (cdr ex)))
;                                                (let ((ex.a (syntax-unwrap a))
;                                                      (ex.d (syntax-unwrap d)))
;                                                  (syntax-dismantle-clause
;                                                    a ex.a skip p.a (syntax-dismantle-clause
;                                                                      d ex.d skip p.d body ...))))
;                                              (skip)))
;    ((_ x ex skip p.var       body ...) (identifier? #'p.var)
;                                        #'(let ((p.var x)) body ...))
;    ((_ x ex skip literal     body ...) (not (vector? (syntax-e #'literal)))
;                                        #'(if (equal? ex 'literal)
;                                              (begin body ...)
;                                              (skip)))))

(define env.syntax
  (value-alist->env
    (aquote
      current-mark-level syntax-prune-level syntax-transcribe
      syntax-note syntax-note-set syntax-note-add syntax-unwrap syntax->datum datum->syntax
      identifier? identifier?! identifier=?
      env.empty make-env env:ref/k env-read-only env-conjoin env-remove env-freeze
      env-describe env-ref/k env-ref env-set! env-freeze!)))

(define env.meta
  (let ((env (make-env))
        (b*.def
          (list
            (cons 'set-vocabulary!    (definition-operator-parser parse-set-vocabulary!    3 #f))
            (cons 'add-vocabulary!    (definition-operator-parser parse-add-vocabulary!    3 #f))
            (cons 'update-vocabulary! (definition-operator-parser parse-update-vocabulary! 3 #f))
            (cons 'remove-vocabulary! (definition-operator-parser parse-remove-vocabulary! 2 #f))
            (cons 'define-vocabulary  (definition-operator-parser parse-define-vocabulary  3 #f))
            (cons 'define-syntax      (definition-operator-parser parse-define-syntax      2 #f))))
        (b*.expr
          (list
            (cons 'quote-syntax        (expression-operator-parser parse-quote-syntax        1 1))
            (cons 'current-environment (expression-operator-parser parse-current-environment 0 0))))
        (b*.qqs '(unsyntax unsyntax-splicing))
        (b*.qqs-and-expr
          (list (cons 'quasiquote-syntax (expression-operator-parser parse-quasiquote-syntax 1 1))))
        (b*.def-and-expr
          (list
            (list 'begin-meta
                  (definition-operator-parser parse-begin-meta-definition 0 #f)
                  (expression-operator-parser parse-begin-meta-expression 1 #f)))))
    (for-each (lambda (id op.def op.expr) (env-vocabulary-bind! env id
                                                                vocab.definition-operator op.def
                                                                vocab.expression-operator op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id op) (env-vocabulary-bind! env id vocab.definition-operator op))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id) (env-vocabulary-bind! env id vocab.quasiquote-syntax id)) b*.qqs)
    (for-each (lambda (id op) (env-vocabulary-bind! env id
                                                    vocab.expression-operator op
                                                    vocab.quasiquote-syntax   id))
              (map car b*.qqs-and-expr) (map cdr b*.qqs-and-expr))
    (for-each (lambda (id op) (env-vocabulary-bind! env id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (env-freeze env)))
