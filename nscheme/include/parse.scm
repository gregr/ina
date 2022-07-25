;;;;;;;;;;;;;;;;;;;
;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;

(define vocab.definition 'definition)
(define vocab.expression 'expression)
(define vocab.set!       'set!)
(define vocab.quasiquote 'quasiquote)
(define vocab.template   'template)
(define vocab.match      'match)

;; Other vocabulary ideas
;(define vocab.module     'module)
;(define vocab.grammar    'grammar)
;(define vocab.formula    'formula)
;(define vocab.term       'term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define (expression-parser proc)    (svector 'expression-parser proc))
(define (expression-parser-proc dp) (expression-parser?! dp) (svector-ref dp 1))
(define (expression-parser? x)      (and (svector? x)
                                         (= (svector-length x) 2)
                                         (eq? (svector-ref x 0) 'expression-parser)))
(define (expression-parser?! x)     (has-type?! expression-parser? 'expression-parser? x))

(define (definition-parser proc)    (svector 'definition-parser proc))
(define (definition-parser-proc dp) (definition-parser?! dp) (svector-ref dp 1))
(define (definition-parser? x)      (and (svector? x)
                                         (= (svector-length x) 2)
                                         (eq? (svector-ref x 0) 'definition-parser)))
(define (definition-parser?! x)     (has-type?! definition-parser? 'definition-parser? x))

(define (parse* env e*) (map (lambda (e) (parse env e)) e*))

(define (parse env expr)
  (define (default)
    (let ((op.default (env-ref env vocab.expression '||)))
      (cond ((procedure? op.default)   ((op.default env `(#f ,expr))))
            ((identifier? expr)        (parse-unbound-identifier env expr))
            ((pair? (syntax-unwrap e)) (raise-syntax-error "invalid procedure application context" expr))
            (else                      (raise-syntax-error "invalid literal context" expr)))))
  (define (operate e.op)
    (let ((op (env-ref^ env vocab.expression e.op)))
      (if (procedure? op)
          (op env expr)
          (default))))
  (let ((pv (syntax-provenance expr)))
    (ast-provenance-add
      (cond ((identifier? expr) (operate expr))
            (else (let ((x (syntax-unwrap e)))
                    (cond ((pair?              x) (let ((e.op (car x)))
                                                    (if (identifier? e.op)
                                                        (operate e.op)
                                                        (default))))
                          ((literal?           x) (default))
                          ((expression-parser? x) ((expression-parser-proc x) env))
                          (else                   (raise-syntax-error "not an expression" expr))))))
      pv)))

(define (parse-unbound-identifier env e)
  ;; TODO: first look for a restart to invoke
  (raise-syntax-error "unbound identifier" e))

(define (parse-default env e)
  (define (fail) (raise-syntax-error "not a literal or procedure application form" e))
  (let ((pv (syntax-provenance e)))
    (ast-provenance-add
      (cond ((syntax->list? e) => (lambda (e*)
                                    (when (null? e*) (fail))
                                    (apply ast:call #f (parse* env e*))))
            ((identifier?   e) (parse-unbound-identifier env e))
            (else              (let ((x (syntax-unwrap e)))
                                 (if (literal? x) (ast:quote #f x) (fail)))))
      pv)))

(define ((keyword-expression-parser parser argc.min argc.max) env expr)
  (cond ((syntax->list? expr) => (lambda (e*)
                                   (let ((argc (- (length e*) 1)))
                                     (unless (<= argc.min argc)
                                       (raise-syntax-error "too few keyword arguments" expr))
                                     (unless (<= argc (or argc.max argc))
                                       (raise-syntax-error "too many keyword arguments" expr))
                                     (apply parser env (cdr e*)))))
        (else (raise-syntax-error "not a list" expr))))

(define ((parse-variable-ref/address addr) env e) (ast:ref (syntax-provenance e) addr))

(define (parse-quote        env e)           (ast:quote (syntax-provenance e) (syntax->datum e)))
(define (parse-quote-syntax env e)           (ast:quote (syntax-provenance e) e))
(define (parse-if           env e.c e.t e.f) (ast:if #f (parse env e.c) (parse env e.t) (parse env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (parse-case-lambda env . e*.cc)
  (define (fail) (raise-syntax-error "not a case-lambda clause" e.cc))
  (define (parse-case-lambda-clause env e.cc)
    (cond ((syntax->list? e.cc)
           => (lambda (e*)
                (when (or (null? e*) (null? (cdr e*))) (fail))
                (let ((e*~.param (syntax->improper-list (car e*)))
                      (e.body    (cdr (syntax-unwrap e.cc))))
                  (let ((e*.param (improper-list->list e*~.param)))
                    (unless (andmap identifier? e*.param)
                      (raise-syntax-error "formal parameter names must be identifiers" (car e*)))
                    (when (null? (cdr e*))
                      (raise-syntax-error "case-lambda clause cannot have an empty body" e.cc))
                    (unless (bound-identifiers-unique? e*.param)
                      (raise-syntax-error "duplicate formal parameter names" (car e*)))
                    (let* ((param (improper-list-map (lambda (e) (fresh-address e)) e*~.param))
                           (addr* (improper-list->list param)))
                      (let ((env.scope (make-env)))
                        (for-each (lambda (i a) (env-bind! env.scope i a)) e*.param addr*)
                        (for-each (lambda (a)   (env-set!  env.scope vocab.expression
                                                           a (parse-variable-ref/address a)))
                                  addr*)
                        (case-clause param (parse-body (env-extend env env.scope) e.body))))))))
          (else (fail))))
  (ast:case-lambda #f (map (lambda (e.cc) (parse-case-lambda-clause env e.cc)) e*.cc)))

(define (env-introduce  env.scope stx.id) (car (env-introduce* env.scope (list stx.id))))
(define (env-introduce* env.scope stx*.id)
  (for-each (lambda (stx.id) (unless (identifier? stx.id)
                               (raise-syntax-error "not an identifier" stx.id)))
            stx*.id)
  (map (lambda (stx.id)
         (when (env-address env.scope stx.id)
           (raise-syntax-error "name defined multiple times" stx.id))
         (let ((addr (fresh-address stx.id)))
           (env-bind! env.scope stx.id addr)
           addr))
       stx*.id))

(define defstate.empty '())

(define (defstate-definitions    dst)      (reverse (if (caar dst) dst (cdr dst))))
(define (defstate-expression     dst)      (if (caar dst) #f (cdar dst)))
(define (defstate-add-expression dst ^ast) (defstate-define dst #f ^ast))

(define (defstate-define dst addr ^ast)
  (if (or (null? dst) (caar dst))
      (cons (cons addr ^ast) dst)
      (cons (cons addr (let ((^ast.prev (cdar dst)))
                         (lambda () (ast:seq #f (^ast.prev) (^ast)))))
            (cdr dst))))

(define (definitions->binding-pairs defs)
  (map binding-pair (map car defs) (map (lambda (^ast) (^ast)) (map cdr defs))))

(define ((keyword-definition-parser parser argc.min argc.max) dst env.scope env stx)
  (cond ((syntax->list? stx) => (lambda (stx*)
                                  (let ((argc (- (length stx*) 1)))
                                    (unless (<= argc.min argc)
                                      (raise-syntax-error "too few keyword arguments" stx))
                                    (unless (<= argc (or argc.max argc))
                                      (raise-syntax-error "too many keyword arguments" stx))
                                    (apply parser dst env.scope env (cdr stx*)))))
        (else (raise-syntax-error "not a list" stx))))

(define (parse-body env e.body)
  (let ((stx* (syntax->list? e.body)))
    (if (null? (cdr stx*))
        (parse env (car stx*))
        (let* ((env.scope (make-env))
               (env       (env-extend env env.scope))
               (dst       (foldl (lambda (stx dst) (parse-definition dst env.scope env stx))
                                 defstate.empty stx*)))
          (unless (defstate-expression dst)
            (raise-syntax-error "no expression after definitions" e.body))
          (ast:letrec (syntax-provenance e.body)
                      (definitions->binding-pairs (defstate-definitions dst))
                      ((defstate-expression dst)))))))

(define (parse-definition dst env.scope env stx)
  (define (default) (defstate-add-expression dst (lambda () (parse env expr))))
  (define (operate stx.op)
    (let ((op (env-ref^ env vocab.definition stx.op)))
      (if (procedure? op)
          (op dst env.scope env stx)
          (default))))
  (cond ((identifier? stx) (operate stx))
        (else (let ((x (syntax-unwrap stx)))
                (cond ((pair? x)              (let ((stx.op (car x)))
                                                (if (identifier? stx.op)
                                                    (operate stx.op)
                                                    (default))))
                      ((definition-parser? x) ((definition-parser-proc x) dst env.scope env))
                      (else                   (default)))))))

(define (parse-begin-meta dst env.scope env stx)
  (cond ((syntax->list? stx)
         => (lambda (stx*)
              (let ((dst (apply parse-begin-definition defstate.empty (cdr stx*))))
                (let* ((bpair*  (definitions->binding-pairs (defstate-definitions dst)))
                       (addr*   (map binding-pair-lhs bpair*))
                       (result* (ast-eval (ast:letrec
                                            (syntax-provenance stx) bpair*
                                            (ast:seq #f ((defstate-expression dst))
                                                     (ast:list #f (map (lambda (a) (ast:ref #f a))
                                                                       addr*)))))))
                  (for-each
                    (lambda (addr result)
                      (env-set! env.scope vocab.expression
                                addr (lambda (env e) (ast:quote (syntax-provenance e) result))))
                    addr* result*)))))
        (else (raise-syntax-error "not a list" stx)))
  dst)

(define (parse-introduce dst env.scope env . stx*) (env-introduce* env.scope stx*) dst)

(define (parse-define dst env.scope env e.lhs . e*.rhs)
  (let loop ((e.lhs e.lhs) (e*.rhs e*.rhs))
    (cond ((identifier? e.lhs)
           (unless (= (length e*.rhs) 1)
             (raise-syntax-error "multiple expressions in definition body" e*.rhs))
           (let ((addr (env-introduce env.scope e.lhs)))
             (env-set! env.scope vocab.expression addr (parse-variable-ref/address addr))
             (defstate-define dst addr (lambda () (parse env (car e*.rhs))))))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (list (expression-parser
                                                 (lambda (env)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs))))))))

;; The right-hand-side expression of declare-parser must evaluate to a procedure which takes the
;; current environment, and produces a parser.  This gives the parser access to its definition
;; environment.
(define (parse-declare-parser dst env.scope env e.vocab e.lhs . e*.rhs)
  (let loop ((e.lhs e.lhs) (e*.rhs e*.rhs))
    (cond ((identifier? e.lhs)
           (unless (= (length e*.rhs) 1)
             (raise-syntax-error "multiple expressions in declaration body" e*.rhs))
           (let* ((addr   (env-address env e.lhs))
                  (vocab  (ast-eval (parse env e.vocab)))
                  (parser ((ast-eval (parse env (car e*.rhs))) env)))
             (env-set! env.scope vocab addr parser)))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (list (expression-parser
                                                 (lambda (env)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
  dst)

(define (transcribe-and-parse parse env.use env.op op stx)
  (define ((lookup/env env) vocab e.id) (env-ref env vocab (env-address env e.id)))
  (let* ((transcription (op (syntax-mark stx antimark)))
         (stx           (syntax-provenance-add
                          (if (procedure? transcription)
                              (transcription (lookup/env env.op) (lookup/env env.use))
                              transcription)
                          stx)))
    (unless (hygienic? stx) (error "unhygienic transcription" stx))
    (let ((m (fresh-mark)))
      (parse (env-extend (env-mark env.op m) env.use) (syntax-mark stx m)))))

(define (transcribe-and-parse-expression env env.op op stx)
  (transcribe-and-parse parse env env.op op stx))

(define (transcribe-and-parse-definition dst env.scope env.use env.op op stx)
  (transcribe-and-parse (lambda (env stx) (parse-definition dst env.scope env stx))
                        env.use env.op op stx))

(define (parse-define-syntax dst env.scope env e.lhs . e*.rhs)
  (let loop ((e.lhs e.lhs) (e*.rhs e*.rhs))
    (cond ((identifier? e.lhs)
           (unless (= (length e*.rhs) 1)
             (raise-syntax-error "multiple expressions in definition body" e*.rhs))
           (env-introduce env.scope e.lhs)
           (let ((op (ast-eval (parse env (car e*.rhs)))))
             (parse-declare-parser
               dst (parse-quote env vocab.expression)
               e.lhs (lambda (env.op)
                       (lambda (env.use stx)
                         (transcribe-and-parse-expression env.use env.op op stx))))
             (parse-declare-parser
               dst (parse-quote env vocab.definition)
               e.lhs (lambda (env.op)
                       (lambda (dst.use env.scope env.use stx)
                         (transcribe-and-parse-definition dst.use env.scope env.use env.op op stx))))))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (list (expression-parser
                                                 (lambda (env)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
  dst)

(define (parse-begin-definition dst env.scope env . stx*)
  (foldl (lambda (stx dst) (parse-definition dst env.scope env stx)) dst stx*))

(define (parse-begin-expression env . e*)
  (let loop ((e (car e*)) (e* (cdr e*)))
    (cond ((null? e*) (parse env e))
          (else       (ast:seq #f (parse env e) (loop (car e*) (cdr e*)))))))

(define (parse-splicing-local-definition dst env.scope env local-body . stx*)
  (cond ((syntax->list? local-body)
         => (lambda (stx*.local-body)
              (let* ((env.scope.inner (make-new))
                     (env (env-extend env env.scope.inner))
                     (dst (apply parse-begin-definition dst env.scope.inner env stx*.local-body)))
                (apply parse-begin-definition dst env.scope env stx*))))
        (else (raise-syntax-error "not a list of definitions" local-body))))

(define (parse-binding-pairs e.bpairs)
  (define (parse-binding-pair e.bpair)
    (cond ((syntax->list? e.bpair)
           => (lambda (e*)
                (unless (= (length e*) 2)
                  (raise-syntax-error "binding pair list length must be 2" e.bpair))
                (unless (identifier? (car e*))
                  (raise-syntax-error "left element of binding pair must be an identifier" e.bpair))
                (cons (car e*) (cadr e*))))
          (else (raise-syntax-error "not a binding pair" e.bpair))))
  (cond ((syntax->list? e.bpairs) => (lambda (e*) (map parse-binding-pair e*)))
        (else (raise-syntax-error "not a list of binding pairs" e.bpairs))))

(define (parse-splicing-let-definition dst env.scope env e.bpairs . stx*)
  (let ((bpair* (parse-binding-pairs e.bpairs)))
    (let* ((env.scope.inner (make-env))
           (dst (foldl (lambda (e.lhs e.rhs dst)
                         (parse-define dst env.scope.inner env e.lhs e.rhs))
                       dst (map car bpair*) (map cdr bpair*))))
      (apply parse-begin-definition dst env.scope (env-extend env env.scope.inner) stx*))))

(define (parse-splicing-let-syntax-definition dst env.scope env e.bpairs . stx*)
  (let ((bpair* (parse-binding-pairs e.bpairs)))
    (let* ((env.scope.inner (make-env))
           (dst (foldl (lambda (e.lhs e.rhs dst)
                         (parse-define-syntax dst env.scope.inner env e.lhs e.rhs))
                       dst (map car bpair*) (map cdr bpair*))))
      (apply parse-begin-definition dst env.scope (env-extend env env.scope.inner) stx*))))

(define (parse-splicing-expression parse-splicing env . args)
  (parse-let env '() (definition-parser (lambda (dst env.scope env)
                                          (apply parse-splicing dst env.scope env args)))))
(define (parse-splicing-local-expression env . args)
  (parse-splicing-expression parse-splicing-local-definition env args))
(define (parse-splicing-let-expression env . args)
  (parse-splicing-expression parse-splicing-let-definition env args))
(define (parse-splicing-let-syntax-expression env . args)
  (parse-splicing-expression parse-splicing-let-syntax-definition env args))

;; TODO: could define these via macro
(define (parse-splicing-let*-definition dst env.scope env e.bpairs . stx*)
  (let ((_ (parse-binding-pairs e.bpairs)))
    (let loop ((dst dst) (env.scope env.scope) (env env) (e.bpairs e.bpairs))
      (let ((x (syntax-unwrap e.bpairs)))
        (cond ((null? x) (apply parse-splicing-let-definition dst env.scope env '() stx*))
              (else      (apply parse-splicing-let-definition dst env.scope env
                                (list (car x))
                                (definition-parser (lambda (dst env.scope env)
                                                     (loop dst env.scope env (cdr x)))))))))))
(define (parse-splicing-let*-expression env . args)
  (parse-splicing-expression parse-splicing-let*-definition env args))

(define (parse-invalid-expression env e)
  (raise-syntax-error "cannot be used in an expression context" e))

(define (parse-let env e0 e1 . e*)
  (cond ((identifier? e0)
         (let ((bpair* (parse-binding-pairs e1)))
           (let ((e.proc (expression-parser (lambda (env)
                                              (apply parse-lambda env (map car bpair*) e*)))))
             (apply ast:call #f (parse-letrec* env (list e0 e.proc) e0)
                    (parse* env (map cdr bpair*))))))
        (else (let ((bpair* (parse-binding-pairs e0)))
                (apply ast:call #f (apply parse-lambda env (map car bpair*) e1 e*)
                       (parse* env (map cdr bpair*)))))))

;; TODO: could define these via macro
(define (parse-non-splicing-expression parse-splicing env e0 e*)
  (apply parse-splicing env e0 (expression-parser (lambda (env) (apply parse-let env '() e*)))))
(define (parse-let* env e.bpairs . e*.body)
  (parse-non-splicing-expression parse-splicing-let*-expression env e.bpairs e*.body))
(define (parse-let-syntax env e.bpairs . e*.body)
  (parse-non-splicing-expression parse-splicing-let-syntax-expression env e.bpairs e*.body))
(define (parse-local env e.defs . e*.body)
  (parse-non-splicing-expression parse-splicing-local-expression env e.defs e*.body))




(define (parse-letrec . args) (apply parse-letrec* args))

(define (parse-letrec* env e.bpairs . e*.body)
  (let ((bpair* (parse-binding-pairs e.bpairs)))
    (apply parse-local (map (lambda (e.lhs e.rhs)
                              (definition-parser
                                (lambda (dst env.scope env)
                                  (parse-define dst env.scope env e.lhs e.rhs))))
                            (map car bpair*) (map cdr bpair*))
           e*.body)))

(define (parse-letrec-syntax env e.bpairs . e*.body)
  (let ((bpair* (parse-binding-pairs e.bpairs)))
    (apply parse-local (map (lambda (e.lhs e.rhs)
                              (definition-parser
                                (lambda (dst env.scope env)
                                  (parse-define-syntax dst env.scope env e.lhs e.rhs))))
                            (map car bpair*) (map cdr bpair*))
           e*.body)))

(define initial.expression
  (list
    '||              (keyword-expression-parser parse-default       1 1)
    'quote           (keyword-expression-parser parse-quote         1 1)
    'quote-syntax    (keyword-expression-parser parse-quote-syntax  1 1)
    'if              (keyword-expression-parser parse-if            3 3)
    'case-lambda     (keyword-expression-parser parse-case-lambda   0 #f)
    'letrec*         (keyword-expression-parser parse-letrec*       2 #f)
    'let-syntax      (keyword-expression-parser parse-let-syntax    2 #f)
    'local           (keyword-expression-parser parse-local         2 #f)
    'lambda          (keyword-expression-parser parse-lambda        2 #f)
    'let             (keyword-expression-parser parse-let           2 #f)
    'let*            (keyword-expression-parser parse-let*          2 #f)
    'letrec          (keyword-expression-parser parse-letrec        2 #f)
    'letrec-syntax   (keyword-expression-parser parse-letrec-syntax 2 #f)

    ;'cond
    ;'case
    ;'when
    ;'unless
    ;'and
    ;'or
    ;'dismantle
    ;'match

    ;'let-values
    ;'let*-values
    ;'letrec-values
    ;'letrec*-values

    ;'dismantle-lambda
    ;'dismantle-let
    ;'dismantle-let*
    ;'dismantle-letrec
    ;'dismantle-letrec*

    ;'match-lambda
    ;'match-let
    ;'match-let*
    ;'match-letrec
    ;'match-letrec*

    ;'syntax-dismantle
    ;'syntax-match
    ;'syntax-case
    ;'syntax-rules
    ))

(define initial.quasiquote
  (list
    'unquote
    'unquote-splicing
    ))

(define initial.quasiquote-and-expression
  (list
    'quasiquote
    ))

(define initial.quasisyntax
  (list
    'unsyntax
    'unsyntax-splicing
    ))

(define initial.quasisyntax-and-expression
  (list
    'quasisyntax
    ))

(define initial.expression.primitive
  (list
    'cons
    'car
    'cdr
    etc.
    ))

(define initial.definition
  (list
    'begin-meta     parse-begin-meta
    'define         (keyword-definition-parser parse-define         2 #f)
    'introduce      (keyword-definition-parser parse-introduce      0 #f)
    'declare-alias  (keyword-definition-parser parse-declare-alias  2 #f)
    'declare-parser (keyword-definition-parser parse-declare-parser 3 #f)
    'define-syntax  (keyword-definition-parser parse-define-syntax  2 #f)

    ;'define-values
    ;'match-define
    ))

(define initial.definition-and-expression
  (list
    'begin
    (keyword-definition-parser parse-begin-definition               0 #f)
    (keyword-expression-parser parse-begin-expression               1 #f)
    'splicing-local
    (keyword-definition-parser parse-splicing-local-definition      2 #f)
    (keyword-expression-parser parse-splicing-local-expression      2 #f)
    'splicing-let
    (keyword-definition-parser parse-splicing-let-definition        2 #f)
    (keyword-expression-parser parse-splicing-let-expression        2 #f)
    ;'splicing-let*
    ;'splicing-letrec
    ;'splicing-letrec*

    'splicing-let-syntax
    (keyword-definition-parser parse-splicing-let-syntax-definition 2 #f)
    (keyword-expression-parser parse-splicing-let-syntax-expression 2 #f)

    ;'splicing-letrec-syntax
    ))

;; TODO: automate populating definition and quasiquote syntax with parse-invalid-expression
