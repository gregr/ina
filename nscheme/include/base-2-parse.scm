;; TODO:
;; - match
;;   - match-define match-lambda match-lambda* match-lambda** match-let match-let*
;;     match-letrec match-letrec*?
;;   - Maybe: syntax-match and/or syntax-rewrite
;; - Maybe: declare-parser define-syntax syntax quasisyntax syntax-dismantle syntax-case

;(define vocab.syntax-pattern  'syntax-pattern)  ; e.g., literals and the wildcard _
;(define vocab.syntax-template 'syntax-template) ; e.g., bound template variables
;; TODO: unsyntax and unsyntax-splicing as quasisyntax-template auxiliaries?

;(define vocab.match          'match)  ; e.g., the wildcard _
;(define vocab.match-operator 'match-operator)

;; Other vocabulary ideas
;(define vocab.module  'module)
;(define vocab.grammar 'grammar)
;(define vocab.formula 'formula)
;(define vocab.term    'term)

(define (parse-begin-meta-definition dst env.scope env stx)
  (let* ((dst      ((definition-operator-parser parse-begin-definition 0 #f)
                    dst env.scope env stx))
         (def*     (defstate-definition* dst))
         (bpair*   (definition*->binding-pair* def*))
         (assign!* (definition*->assigner* def*))
         (addr*    (map binding-pair-left bpair*)))
    (for-each (lambda (assign! result) (assign! result))
              assign!* (ast-eval (ast:letrec (syntax-provenance stx) bpair*
                                             ($begin ((defstate-expression dst))
                                                     (apply $list (map $ref addr*)))))))
  dst)

(define (parse-begin-meta-expression env stx)
  ($quote (ast-eval ((expression-operator-parser parse-begin-expression 1 #f) env stx))))

;; The right-hand-side expression of declare-parser must evaluate to a procedure which takes the
;; current environment, and produces a parser.  This gives the parser access to its definition
;; environment.
;(define (parse-declare-parser dst env.scope env e.vocab e.lhs . e*.rhs)
;  (let loop ((e.lhs e.lhs) (e*.rhs e*.rhs))
;    (cond ((identifier? e.lhs)
;           (unless (= (length e*.rhs) 1)
;             (raise-syntax-error "multiple expressions in declaration body" e*.rhs))
;           (let* ((addr   (env-address env e.lhs))
;                  (vocab  (ast-eval (parse-expression env e.vocab)))
;                  (parser ((ast-eval (parse-expression env (car e*.rhs))) env)))
;             (unless addr (raise-syntax-error "unbound identifier" e.lhs))
;             (env-set! env.scope vocab addr parser)))
;          (else (let ((x (syntax-unwrap e.lhs)))
;                  (cond ((pair? x) (loop (car x)
;                                         (list (expression-parser
;                                                 (lambda (env _)
;                                                   ($provenance
;                                                     (apply parse-lambda env (cdr x) e*.rhs)
;                                                     (cdr x)))))))
;                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
;  dst)

;; TODO: do without this (and anything else using ast-eval) until late stage bootstrapping
;(define (parse-define-syntax dst env.scope env e.lhs . e*.rhs)
;  (let loop ((e.lhs e.lhs) (e*.rhs e*.rhs))
;    (cond ((identifier? e.lhs)
;           (unless (= (length e*.rhs) 1)
;             (raise-syntax-error "multiple expressions in definition body" e*.rhs))
;           (env-introduce env.scope e.lhs)
;           (let ((op (ast-eval (parse-expression env (car e*.rhs)))))
;             (parse-declare-parser
;               dst (parse-quote env vocab.expression)
;               e.lhs (lambda (env.op)
;                       (lambda (env.use stx)
;                         (transcribe-and-parse-expression env.use env.op op stx))))
;             (parse-declare-parser
;               dst (parse-quote env vocab.definition)
;               e.lhs (lambda (env.op)
;                       (lambda (dst.use env.scope env.use stx)
;                         (transcribe-and-parse-definition dst.use env.scope env.use env.op op stx))))))
;          (else (let ((x (syntax-unwrap e.lhs)))
;                  (cond ((pair? x) (loop (car x)
;                                         (list (expression-parser
;                                                 (lambda (env _)
;                                                   ($provenance
;                                                     (apply parse-lambda env (cdr x) e*.rhs)
;                                                     (cdr x)))))))
;                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
;  dst)

;; Would syntax-dismantle be helpful enough to justify implementing it?
;(define-syntax syntax-dismantle
;  (syntax-rules ()
;    ((_ x skip clause ...)
;     (let* ((y x) (ex (syntax-unwrap y)))
;       (syntax-dismantle-clauses y ex skip clause ...)))))
;
;(define-syntax syntax-dismantle-clauses
;  (syntax-rules ()
;    ((_ x ex skip) (error "no matching clause" x))
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

;; TODO:
;(define initial.definition
  ;(list
    ;;'declare-parser (definition-operator-parser parse-declare-parser 3 #f)
    ;;'define-syntax  (definition-operator-parser parse-define-syntax  2 #f)
    ;))

(define env.base-2
  (let ((env.scope (make-env))
        (b*.def-and-expr
          (list
            (list 'begin-meta parse-begin-meta-definition parse-begin-meta-expression))))
    (for-each (lambda (id op.def op.expr) (env-bind! env.scope id
                                                     vocab.definition-operator op.def
                                                     vocab.expression-operator op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (env-freeze! env.scope)
    (env-extend env.base-0 env.scope)))
