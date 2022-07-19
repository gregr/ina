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

(define (unique? xs) (= (set-count (list->set xs)) (length xs)))

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
    (let ((op.default (env-ref^ env vocab.expression '||)))
      (cond ((procedure? op.default)   ((op.default env (datum->syntax #f `(#f ,expr)))))
            ((identifier? expr)        (parse-unbound-identifier env expr))
            ((pair? (syntax-unwrap e)) (raise-syntax-error "invalid procedure application context" expr))
            (else                      (raise-syntax-error "invalid literal context" expr)))))
  (define (operate e.op)
    (let ((op (env-ref^ env vocab.expression (identifier-id e.op))))
      (if (procedure? op)
        (op env expr)
        (default))))
  (cond ((identifier? expr) (operate expr))
        (else (let ((x (syntax-unwrap e)))
                (cond ((pair?              x) (let ((e.op (car x)))
                                                (if (identifier? e.op)
                                                  (operate e.op)
                                                  (default))))
                      ((literal?           x) (default))
                      ((expression-parser? x) ((expression-parser-proc x) env))
                      (else                   (raise-syntax-error "not an expression" expr)))))))

(define (parse-unbound-identifier env e)
  ;; TODO: first look for a restart to invoke
  (raise-syntax-error "unbound identifier" e))

(define (parse-default env _ e)
  (define (fail) (raise-syntax-error "not a literal or procedure application form" e))
  (cond ((syntax->list? e) => (lambda (e*)
                                (when (null? e*) (fail))
                                (apply ast:call #f (parse* env e*))))
        ((identifier?   e) (parse-unbound-identifier env e))
        (else (let ((x (syntax-unwrap e)))
                (if (literal? x) (ast:quote #f d) (fail))))))

(define ((keyword-expression-parser parser argc.min argc.max) env expr)
  (let ((pv (syntax-provenance expr)))
    (cond ((syntax->list? expr) => (lambda (e*)
                                     (let ((argc (- (length e*) 1)))
                                       (unless (<= argc.min argc)
                                         (raise-syntax-error "too few keyword arguments" expr))
                                       (unless (<= argc (or argc.max argc))
                                         (raise-syntax-error "too many keyword arguments" expr))
                                       (ast-provenance-add (apply parser env e*) pv))))
          (else (raise-syntax-error "not a keyword application form" expr)))))

(define ((parse-variable-ref/address addr) env e) (ast:ref (syntax-provenance e) addr))

(define (parse-quote        env _ e)           (ast:quote (syntax-provenance e) (syntax->datum e)))
(define (parse-quote-syntax env _ e)           (ast:quote (syntax-provenance e) e))
(define (parse-if           env _ e.c e.t e.f) (ast:if #f (parse env e.c) (parse env e.t) (parse env e.f)))
(define (parse-lambda       env _ p . e*)      (parse-case-lambda env _ (cons p e*)))

(define (parse-case-lambda env _ . e*.cc)
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
                    (let ((id* (map identifier-id e*.param)))
                      (unless (unique? id*)
                        (raise-syntax-error "duplicate formal parameter names" (car e*)))
                      (let* ((param (improper-list-map (lambda (e) (fresh-address (identifier-id e)))
                                                       e*~.param))
                             (addr* (improper-list->list param)))
                        (let ((env.scope (env:scope)))
                          (env-bind!** env.scope id* addr*)
                          (env-set!**  env.scope vocab.expression
                                       addr* (map parse-variable-ref/address addr*))
                          (case-clause param (parse-body (env:inherit env env.scope) e.body)))))))))
          (else (fail))))
  (ast:case-lambda #f (map (lambda (e.cc) (parse-case-lambda-clause env e.cc)) e*.cc)))

(define (defstate env)
  (let* ((id*          (mutable-set))
         (addr*        '())
         (^ast*        '())
         (^ast.current #f)
         (env.scope    (env:scope))
         (env.full     (env:inherit env env.scope))
         (id**         '())
         (env*.scope   '())
         (env*.full    '())
         (meta-frame*  '()))
    (lambda (method)
      (case method
        ((id-add!)               (lambda (e.id)
                                   (let ((id (identifier-id e.id)))
                                     (when (set-member? id* id)
                                       (raise-syntax-error "name defined multiple times" e.id))
                                     (set-add! id* id))))
        ((binding-pair-add!)     (lambda (addr ^ast)
                                   (set! addr* (cons addr addr*))
                                   (set! ^ast* (cons ^ast ^ast*))))
        ((binding-pairs-build)   (map binding-pair
                                      (reverse addr*)
                                      (map (lambda (^ast) (^ast)) (reverse ^ast*))))
        ((current-expr)          ^ast.current)
        ((current-expr-discard!) (set! ^ast.current #f))
        ((current-expr-extend!)  (lambda (^ast)
                                   (set! ^ast.current
                                     (if ^ast.current
                                       (let ((current (^ast.current)))
                                         (lambda () (ast:seq #f (current) (^ast))))
                                       ^ast))))
        ((env-scope)             env.scope)
        ((env-full)              env.full)
        ((env-scope-push!)       (set! id**       (cons id* id**))
                                 (set! id*        (mutable-set))
                                 (set! env*.scope (cons env.scope env*.scope))
                                 (set! env.scope  (env:scope)))
        ((env-full-push! env)    (set! env*.full  (cons env.full env*.full))
                                 (set! env.full   env))
        ((env-scope-pop!)        (set! id*        (car id**))
                                 (set! id**       (cdr id**))
                                 (set! env.scope  (car env*.scope))
                                 (set! env*.scope (cdr env*.scope)))
        ((env-full-pop!)         (set! env.full   (car env*.full))
                                 (set! env*.full  (cdr env*.full)))
        ((meta-push!)            (set! meta-frame*  (cons (vector addr* ^ast* ^ast.current) meta-frame*))
                                 (set! addr*        '())
                                 (set! ^ast*        '())
                                 (set! ^ast.current #f))
        ((meta-pop!)             (let ((mframe (car meta-frame*)))
                                   (set! meta-frame*  (cdr meta-frame*))
                                   (set! addr*        (vector-ref mframe 0))
                                   (set! ^ast*        (vector-ref mframe 1))
                                   (set! ^ast.current (vector-ref mframe 2))))
        (else                    (error "invalid defstate operation" method))))))

(define (defstate-id-add!               dst e.id)      ((dst 'id-add!) e.id))
(define (defstate-binding-pair-add!     dst addr ^ast) ((dst 'binding-pair-add!) id addr ^ast))
(define (defstate-binding-pairs-build   dst)           (dst 'binding-pairs-build))
(define (defstate-current-expr          dst)           (dst 'current-expr))
(define (defstate-current-expr-extend!  dst ^ast)      ((dst 'current-expr-extend!) ^ast))
(define (defstate-current-expr-discard! dst)           (dst 'current-expr-discard!))
(define (defstate-env-scope             dst)           (dst 'env-scope))
(define (defstate-env-full              dst)           (dst 'env-full))
(define (defstate-env-scope-push!       dst)           (dst 'env-scope-push!))
(define (defstate-env-full-push!        dst env)       ((dst 'env-full-push!) env))
(define (defstate-env-full-push-scope!  dst)           (defstate-env-full-push!
                                                         dst (env:inherit (defstate-env-full  dst)
                                                                          (defstate-env-scope dst))))
(define (defstate-env-scope-pop!        dst)           (dst 'env-scope-pop!))
(define (defstate-env-full-pop!         dst)           (dst 'env-full-pop!))
(define (defstate-meta-push!            dst)           (dst 'meta-push!))
(define (defstate-meta-pop!             dst)           (dst 'meta-pop!))

(define (parse-body env e.body)
  (let ((stx* (syntax->list? e.body)))
    (if (null? (cdr stx*))
      (parse env (car stx*))
      (let ((dst (defstate env)))
        (for-each (lambda (stx) (parse-definition dst stx)) stx*)
        (unless (defstate-current-expr dst)
          (raise-syntax-error "no expression after definitions" e.body))
        (ast:letrec #f (defstate-binding-pairs-build dst) ((defstate-current-expr dst)))))))

(define (parse-definition dst stx)
  (define (default) (parse-definition-context-expression dst stx))
  (define (operate stx.op)
    (let ((op (env-ref^ (defstate-env-full dst) vocab.definition (identifier-id stx.op))))
      (if (procedure? op)
        (op dst stx)
        (default))))
  (cond ((identifier? stx) (operate stx))
        (else (let ((x (syntax-unwrap stx)))
                (cond ((pair? x)              (let ((stx.op (car x)))
                                                (if (identifier? stx.op)
                                                  (operate stx.op)
                                                  (default))))
                      ((definition-parser? x) ((definition-parser-proc x) dst))
                      (else                   (default)))))))

(define (parse-definition-context-expression dst expr)
  (let ((env (defstate-env-full dst)))
    (defstate-current-expr-extend! dst (lambda () (parse env expr)))))

(define (parse-begin-meta dst _ . stx*)
  (defstate-meta-push! dst)
  (apply parse-begin-definition dst _ stx*)
  (let* ((bpair*  (defstate-binding-pairs-build dst))
         (addr*   (map binding-pair-lhs bpair*))
         (result* (ast-eval (ast:letrec #f bpair*
                                        (ast:seq #f ((defstate-current-expr dst))
                                                 (ast:list #f (map (lambda (a) (ast:ref #f a))
                                                                   addr*)))))))
    (let ((env.scope (defstate-env-scope dst)))
      (for-each (lambda (addr result) (env-set! env.scope vocab.expression
                                                addr (lambda (env e) (ast:quote #f result))))
                addr* result*)))
  (defstate-meta-pop! dst))

(define (parse-define dst _ e.lhs e.rhs)
  (let loop ((e.lhs e.lhs) (e.rhs e.rhs))
    (cond ((identifier? e.lhs)
           (defstate-id-add! dst e.lhs)
           (let* ((id        (identifier-id e.lhs))
                  (addr      (fresh-address id))
                  (env.scope (defstate-env-scope dst)))
             (env-bind! env.scope id addr)
             (env-set!  env.scope vocab.expression addr (parse-variable-ref/address addr))
             (defstate-binding-pair-add!
               dst addr
               (let ((env (defstate-env-full dst)))
                 (cond ((defstate-current-expr dst)
                        => (lambda (current)
                             (defstate-current-expr-discard! dst)
                             (lambda () (ast:seq #f (current) (parse env e.rhs)))))
                       (else (lambda () (parse env e.rhs))))))))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (datum->syntax
                                           #f (expression-parser
                                                (lambda (env) (parse-lambda env #f (cdr x) e.rhs))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs))))))))

(define (parse-introduce dst _ . stx*)
  (for-each (lambda (stx) (unless (identifier? stx)
                            (raise-syntax-error "not an identifier" stx)))
            stx*)
  (for-each (lambda (stx) (defstate-id-add! dst stx)) stx*)
  (let ((id* (map identifier-id stx*)))
    (env-bind!** env.scope id* (map fresh-address id*))))

;; The right-hand-side expression of declare-parser must evaluate to a procedure which takes the current
;; environment, and produces a parser.  This gives the parser access to its definition environment.
(define (parse-declare-parser dst _ e.vocab e.lhs e.rhs)
  (let loop ((e.lhs e.lhs) (e.rhs e.rhs))
    (cond ((identifier? e.lhs)
           (let* ((id        (identifier-id e.lhs))
                  (env       (defstate-env-full dst))
                  (env.scope (defstate-env-scope dst))
                  (addr      (env-address env id))
                  (vocab     (ast-eval (parse env e.vocab)))
                  (parser    ((ast-eval (parse env e.rhs)) env)))
             (env-set! env.scope vocab addr parser)))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (datum->syntax
                                           #f (expression-parser
                                                (lambda (env) (parse-lambda env #f (cdr x) e.rhs))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs))))))))

(define (transcribe-and-parse parse env.use env.op op stx)
  (define ((lookup/env env) vocab e.id)
    (env-ref env vocab (env-address env (identifier-id e.id))))
  (let* ((transcription (op (syntax-mark stx antimark)))
         (stx           (cond ((procedure? transcription)
                               (transcription (lookup/env env.transcribe) (lookup/env env.use)))
                              ((syntax?    transcription) transcription)
                              (else (error "not syntax or transcription procedure" transcription))))
         (m             (fresh-mark))
         (stx           (syntax-mark stx m)))
    (parse (env:inherit (env:unmark env.transcribe m) env.use) stx)))

(define (transcribe-and-parse-expression env env.op op stx)
  (transcribe-and-parse parse env env.op op stx))

(define (transcribe-and-parse-definition dst env.op op stx)
  (transcribe-and-parse (lambda (env stx)
                          (defstate-env-full-push! dst env)
                          (parse-definition        dst stx)
                          (defstate-env-full-pop!  dst))
                        (defstate-env-full dst) env.op op stx))

(define (parse-define-syntax dst _ e.lhs e.rhs)
  (let loop ((e.lhs e.lhs) (e.rhs e.rhs))
    (cond ((identifier? e.lhs)
           (parse-introduce dst #f e.lhs)
           (let* ((env (defstate-env-full dst))
                  (op  (ast-eval (parse env e.rhs))))
             (parse-declare-parser
               dst #f (parse-quote env #f (datum->syntax #f vocab.expression))
               e.lhs (lambda (env.op)
                       (lambda (env.use stx)
                         (transcribe-and-parse-expression env.use env.op op stx))))
             (parse-declare-parser
               dst #f (parse-quote env #f (datum->syntax #f vocab.definition))
               e.lhs (lambda (env.op)
                       (lambda (dst.use stx)
                         (transcribe-and-parse-definition dst.use env.op op stx))))))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (datum->syntax
                                           #f (expression-parser
                                                (lambda (env) (parse-lambda env #f (cdr x) e.rhs))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs))))))))

(define (parse-begin-definition dst _ . stx*)
  (for-each (lambda (stx) (parse-definition dst stx)) stx*))

(define (parse-begin-expression env _ . e*)
  (let loop ((e (car e*)) (e* (cdr e*)))
    (cond ((null? e*) (parse env e))
          (else       (ast:seq #f (parse env e) (loop (car e*) (cdr e*)))))))

(define (parse-splicing-local-definition dst _ local-body . stx*)
  (cond ((syntax->list? local-body)
         => (lambda (stx*.local-body)
              (defstate-env-scope-push!      dst)
              (defstate-env-full-push-scope! dst)
              (parse-begin-definition        dst #f stx*.local-body)
              (defstate-env-scope-pop!       dst)
              (parse-begin-definition        dst #f stx*)
              (defstate-env-full-pop!        dst)))
        (else (raise-syntax-error "not a list of definitions" local-body))))

(define (parse-splicing-let-definition dst _ e.bpair . stx*)
  (let ((bpair* (parse-binding-pairs e.bpair)))
    (defstate-env-scope-push!      dst)
    (for-each (lambda (e.lhs e.rhs) (parse-define dst #f e.lhs e.rhs))
              (map car bpair*) (map cdr bpair*))
    (defstate-env-full-push-scope! dst)
    (defstate-env-scope-pop!       dst)
    (parse-begin-definition        dst #f stx*)
    (defstate-env-full-pop!        dst)))

(define (parse-splicing-let-syntax-definition dst _ e.bpair . stx*)
  (let ((bpair* (parse-binding-pairs e.bpair)))
    (defstate-env-scope-push!      dst)
    (for-each (lambda (e.lhs e.rhs) (parse-define-syntax dst #f e.lhs e.rhs))
              (map car bpair*) (map cdr bpair*))
    (defstate-env-full-push-scope! dst)
    (defstate-env-scope-pop!       dst)
    (parse-begin-definition        dst #f stx*)
    (defstate-env-full-pop!        dst)))

(define (parse-splicing-expression env parse-splicing args)
  (parse-let env #f (datum->syntax #f '())
             (datum->syntax #f (definition-parser (lambda (dst) (apply parse-splicing dst args))))))
(define (parse-splicing-local-expression env . args)
  (parse-splicing-expression env parse-splicing-local-definition args))
(define (parse-splicing-let-expression env . args)
  (parse-splicing-expression env parse-splicing-let-definition args))
(define (parse-splicing-let-syntax-expression env . args)
  (parse-splicing-expression env parse-splicing-let-syntax-definition args))

;; TODO: no, these should be derived
;(define (parse-splicing-let*-expression env . args)
  ;(parse-splicing-expression env parse-splicing-let*-definition args))
;(define (parse-splicing-letrec-expression env . args)
  ;(parse-splicing-expression env parse-splicing-letrec-definition args))
;(define (parse-splicing-letrec-syntax-expression env . args)
  ;(parse-splicing-expression env parse-splicing-letrec-syntax-definition args))

(define (parse-invalid-expression env e)
  (raise-syntax-error "cannot be used in an expression context" e))

(define (parse-binding-pair e.bpair)
  (cond ((syntax->list? e.bpair)
         => (lambda (e*)
              (unless (= (length e*) 2)
                (raise-syntax-error "binding pair list length must be 2" e.bpair))
              (unless (identifier? (car e*))
                (raise-syntax-error "left element of binding pair must be an identifier" e.bpair))
              (cons (car e*) (cadr e*))))
        (else (raise-syntax-error "not a binding pair" e.bpair))))

(define (parse-binding-pairs e.bpairs)
  (cond ((syntax->list? e.bpairs) => (lambda (e*) (map parse-binding-pair e*)))
        (else (raise-syntax-error "not a list of binding pairs" e.bpairs))))

(define (parse-let env _ e0 e1 . e*)
  (cond ((identifier? e0)
         (let ((bpair* (parse-binding-pairs e1)))
           (let ((e.proc (expression-parser
                           (lambda (env) (apply parse-lambda env #f
                                                (datum->syntax #f (map car bpair*))
                                                e*)))))
             (apply ast:call #f (parse-letrec* env (datum->syntax #f (list e0 e.proc)) e0)
                    (parse* env (map cdr bpair*))))))
        (else (let ((bpair* (parse-binding-pairs e0)))
                (apply ast:call #f (apply parse-lambda env #f
                                          (datum->syntax #f (map car bpair*)) e1 e*)
                       (parse* env (map cdr bpair*)))))))

(define (parse-let* env _ e.bpairs . e*.body)
  )

;; TODO: define via definition-body
(define (parse-local env _ e.defs . e*.body)
  )

;; TODO: define via local
(define (parse-letrec* env e.bpairs . e*.body)

  )

(define (parse-letrec . args) (apply parse-letrec* args))

(define initial.expression
  (list
    '||              (keyword-expression-parser parse-default      1 1)
    'quote           (keyword-expression-parser parse-quote        1 1)
    'quote-syntax    (keyword-expression-parser parse-quote-syntax 1 1)
    'if              (keyword-expression-parser parse-if           3 3)
    'case-lambda     (keyword-expression-parser parse-case-lambda  0 #f)
    'letrec*         (keyword-expression-parser parse-letrec*      2 #f)
    'let-syntax
    'letrec-syntax

    ;'cond
    ;'case
    ;'when
    ;'unless
    ;'and
    ;'or
    ;'dismantle
    ;'match

    'local           (keyword-expression-parser parse-local        2 #f)
    'lambda          (keyword-expression-parser parse-lambda       2 #f)
    'let             (keyword-expression-parser parse-let          2 #f)
    'let*            (keyword-expression-parser parse-let*         2 #f)
    'letrec          (keyword-expression-parser parse-letrec       2 #f)

    ;'let-values
    ;'let*-values
    ;'letrec-values
    ;'letrec*-values


    ;'dismantle-lambda
    ;'dismantle-let
    ;'dismantle-let*
    ;'dismantle-letrec
    ;'dismantle-letrec*
    ;'dismantle-let-values
    ;'dismantle-let*-values
    ;'dismantle-letrec-values
    ;'dismantle-letrec*-values

    ;'match-lambda
    ;'match-let
    ;'match-let*
    ;'match-letrec
    ;'match-letrec*
    ;'match-let-values
    ;'match-let*-values
    ;'match-letrec-values
    ;'match-letrec*-values

    ;'let*-syntax
    ;'let-syntaxes
    ;'let*-syntaxes
    ;'letrec-syntaxes
    ;'letrec-syntaxes+values

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

(define initial.expression.primitive
  (list
    'cons
    'car
    'cdr
    etc.
    ))

(define initial.definition
  (list
    'begin-meta     (keyword-definition-parser parse-begin-meta     0 #f)
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

    ;'splicing-let-values
    ;'splicing-let*-values
    ;'splicing-letrec-values
    ;'splicing-letrec*-values

    'splicing-let-syntax
    (keyword-definition-parser parse-splicing-let-syntax-definition 2 #f)
    (keyword-expression-parser parse-splicing-let-syntax-expression 2 #f)
    ;'splicing-let*-syntax
    ;'splicing-letrec-syntax
    ;'splicing-let-syntaxes
    ;'splicing-let*-syntaxes
    ;'splicing-letrec-syntaxes
    ;'splicing-letrec-syntaxes+values
    ))

;; TODO: automate populating definition and quasiquote syntax with parse-invalid-expression
