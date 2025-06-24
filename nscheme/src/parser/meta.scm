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
;(define vocab.term    'term)

(define (with-higher-mark-level thunk) (current-mark-level (+ (current-mark-level) 1) thunk))

(define (parse-begin-meta-definition* env stx*)
  (let ((E (apply/values $quote-values
                         (with-higher-mark-level
                           (lambda () (E-eval (D->E/publish (parse-begin-definition* env stx*))))))))
    ($d:expression (lambda () E))))
(define (parse-begin-meta-definition env . stx*) (parse-begin-meta-definition* env stx*))

(define (parse-begin-meta-expression* env stx*)
  (apply/values $quote-values
                (with-higher-mark-level
                  (lambda () (E-eval ((operator-parser parse-begin-expression 1 #f) env stx*))))))
(define (parse-begin-meta-expression env . stx*) (parse-begin-meta-expression* env stx*))

(define ((parse-modify-in-vocabulary env-vocabulary-modify!*) env id.lhs . stx*)
  (parse-identifier id.lhs)
  (unless (even? (length stx*))
    (raise-parse-error "not a list of alternating vocabularies and values" stx*))
  (env-vocabulary-modify!*
    env id.lhs (with-higher-mark-level (lambda () (map E-eval (parse-expression* env stx*)))))
  ($d:begin))
(define parse-define-in-vocabulary (parse-modify-in-vocabulary env-vocabulary-bind!*))
(define parse-add-in-vocabulary    (parse-modify-in-vocabulary env-vocabulary-add!*))

(define (parse-define-syntax env.op stx.lhs . stx*.rhs)
  (define (finish id.lhs ^rhs)
    (parse-identifier id.lhs)
    (let ((op (with-higher-mark-level (lambda () (E-eval (^rhs env.op))))))
      (env-vocabulary-bind!
        env.op id.lhs
        vocab.expression
        (lambda (env.use stx) ((syntax-transcribe/parse #f parse-expression) stx op env.op env.use))
        vocab.definition
        (lambda (env.use stx) ((syntax-transcribe/parse #t parse-definition) stx op env.op env.use))))
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
      env.empty make-env env-read-only env-read-and-write env-conjoin env-remove env-freeze
      env-describe env-ref/k env-ref env-bind!/k env-rebind! env-bind! env-freeze!)))

(define env.meta
  (let ((env (make-env))
        (b*.def-and-expr
          (list (list 'begin-meta
                      (operator-parser parse-begin-meta-definition 0 #f)
                      (operator-parser parse-begin-meta-expression 1 #f)))))
    (for-each (lambda (id op.def op.expr)
                (env-vocabulary-bind! env id vocab.definition op.def vocab.expression op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (alist-for-each
      (list (cons 'define-in-vocabulary (operator-parser parse-define-in-vocabulary 1 #f))
            (cons 'add-in-vocabulary    (operator-parser parse-add-in-vocabulary    1 #f))
            (cons 'define-syntax        (operator-parser parse-define-syntax        2 #f)))
      (lambda (id op) (env-vocabulary-bind! env id vocab.definition op)))
    (env-freeze env)))
