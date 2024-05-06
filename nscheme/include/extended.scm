;; TODO:
;; - match
;;   - match-define match-lambda match-lambda* match-lambda** match-let match-let*
;;     match-letrec match-letrec*?
;;   - Maybe: syntax-match and/or syntax-rewrite
;; - Maybe: declare-parser define-syntax syntax quasisyntax syntax-dismantle syntax-case

;(define vocab.syntax-pattern  'syntax-pattern)  ; e.g., literals and the wildcard _
;(define vocab.syntax-template 'syntax-template) ; e.g., bound template variables
;; TODO: unsyntax and unsyntax-splicing as quasisyntax-template auxiliaries?

;; Other vocabulary ideas
;(define vocab.module  'module)
;(define vocab.grammar 'grammar)
;(define vocab.formula 'formula)
;(define vocab.term    'term)

(define (make-program)
  (mlet ((D.current ($d:begin)))
    (lambda (method)
      (case method
        ((add!)    (lambda (D.new) (set! D.current ($d:begin D.current D.new))))
        ((eval!)   (let ((E ((D->E/eval E-eval) D.current)))
                     (set! D.current ($d:expression (lambda () E)))))
        ((current) D.current)))))

(define (program->D    p) (p 'current))
(define (program->E    p) (D->E (program->D p)))
(define (program-eval! p) (p 'eval!))
(define (program-eval  p) (program-eval! p) (E-eval (program->E p)))

(define (program-link-definition*/env.d p env.d env stx*.def)
  ((p 'add!) (parse-begin-definition* env.d (env-conjoin env.d env) stx*.def)))

(define (program-link-definition* p env stx*.def)
  (let ((env.d (make-env)))
    (program-link-definition*/env.d p env.d env stx*.def)
    (env-freeze env.d)))

(define (eval-definition* env stx*.def)
  (let* ((p     (make-program))
         (env.d (program-link-definition* p env stx*.def)))
    (program-eval! p)
    env.d))

(define (parse-begin-meta-definition env.d env stx)
  (let* ((D       (parse-begin-definition* env.d env (syntax->list stx)))
         (dst.new (D->defstate D))
         (E       ((defstate->E/eval E-eval) dst.new)))
    (if (defstate-expression dst.new)
        ($d:expression (lambda () E))
        ($d:define (make-env) '_ (lambda () E)))))

(define (parse-begin-meta-expression env stx)
  (apply/values $quote-values
                (E-eval ((expression-operator-parser parse-begin-expression 1 #f) env stx))))

(define (parse-current-environment env) ($quote env))

(define (parse-declare-vocabulary-value env.d env id.lhs stx.vocab stx.rhs)
  (parse-identifier id.lhs)
  (let ((vocab    (E-eval (parse-expression env stx.vocab)))
        (rhs      (E-eval (parse-expression env stx.rhs)))
        (vocab=>v (or (env-ref env id.lhs) vocab-dict.empty)))
    (env-set! env.d id.lhs (vocab-dict-set vocab=>v vocab rhs)))
  ($d:begin))

(define (parse-define-syntax env.d env.op stx.lhs . stx*.rhs)
  (define (finish id.lhs ^rhs)
    (env-introduce! env.d id.lhs)
    (let ((op (E-eval (^rhs env.op))))
      (env-set^! env.d id.lhs
                 vocab.expression-operator
                 (lambda (env.use stx)
                   (transcribe-and-parse-expression env.use env.op op stx))
                 vocab.definition-operator
                 (lambda (env.d.use env.use stx)
                   (transcribe-and-parse-definition env.d.use env.use env.op op stx))))
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

(define env.extended
  (let ((env (make-env))
        (b*.def
          (list
            (cons 'declare-vocabulary-value
                  (definition-operator-parser parse-declare-vocabulary-value 3 3))
            (cons 'define-syntax (definition-operator-parser parse-define-syntax 2 #f))))
        (b*.expr
          (list
            (cons 'current-environment
                  (expression-operator-parser parse-current-environment 0 0))))
        (b*.def-and-expr
          (list
            (list 'begin-meta parse-begin-meta-definition parse-begin-meta-expression))))
    (for-each (lambda (id op.def op.expr) (env-bind! env id
                                                     vocab.definition-operator op.def
                                                     vocab.expression-operator op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id op) (env-bind! env id vocab.definition-operator op))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id op) (env-bind! env id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (env-freeze env)))
