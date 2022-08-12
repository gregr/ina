;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;

(define vocab.definition          'definition)
(define vocab.definition:operator 'definition:operator)
(define vocab.expression          'expression)
(define vocab.expression:operator 'expression:operator)
(define vocab.expression:keyword  'expression:keyword)

(define vocab.set!       'set!)
(define vocab.quasiquote 'quasiquote)
(define vocab.template   'template)
(define vocab.match      'match)

;; Other vocabulary ideas
;(define vocab.module     'module)
;(define vocab.grammar    'grammar)
;(define vocab.formula    'formula)
;(define vocab.term       'term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identifier->fresh-address p) (fresh-address (identifier-unqualify p)))

(define ($quote value)       (ast:quote #f value))
(define ($ref   addr)        (ast:ref   #f addr))
(define ($call  proc . args) (ast:call  #f proc args))
(define ($if    c t f)       (ast:if #f c t f))

(define ($case-lambda-clause param*~ addr*->body)
  (let ((addr*~ (improper-list-map identifier->fresh-address param*~)))
    (case-lambda-clause addr*~ (apply addr*->body (improper-list->list addr*~)))))

(define ($case-lambda . cc*)
  (ast:case-lambda #f (map (lambda (cc) ($case-lambda-clause (car cc) (cdr cc))) cc*)))

(define ($lambda param*~     addr*->body) ($case-lambda (cons param*~ addr*->body)))
(define ($let    param* rhs* addr*->body) (apply $call ($lambda param* addr*->body) rhs*))
(define ($letrec param* addr*->rhs*&body)
  (let* ((addr*     (map identifier->fresh-address param*))
         (rhs*&body (apply addr*->rhs*&body addr*)))
    (ast:letrec #f (map binding-pair addr* (car rhs*&body)) (cdr rhs*&body))))

(define $and
  (case-lambda
    (()       ($quote #t))
    ((a . a*) (let loop ((a a) (a* a*))
                (cond ((null? a*) a)
                      (else       ($if a (loop (car a*) (cdr a*)) ($quote #f))))))))

(define $or
  (case-lambda
    (()       ($quote #f))
    ((a . a*) (let loop ((a a) (a* a*))
                (cond ((null? a*) a)
                      (else ($let '(t) (list a)
                                  (lambda (addr.t)
                                    ($if ($ref addr.t)
                                         ($ref addr.t)
                                         (loop (car a*) (cdr a*)))))))))))

(define ($when   c body . body*) ($if c (apply $begin body body*) ($call ($quote values))))
(define ($unless c body . body*) (apply $when ($not c) body body*))

(define ($not  x) ($if x ($quote #f) ($quote #t)))
(define ($pcall prim . args) (apply $call ($quote prim) args))

(define ($begin  a . a*) (foldr (lambda (a0 a1) ($pcall call-with-values ($lambda '() a0)
                                                        ($lambda '() (lambda _ a1))))
                                a a*))
(define ($begin* a . a*) (foldr (lambda (a0 a1) ($pcall call-with-values ($lambda '() a0)
                                                        ($lambda #f  (lambda _ a1))))
                                a a*))

(define $void         ($pcall values))
(define ($null? x)    ($pcall null? x))
(define ($pair? x)    ($pcall pair? x))
(define ($cons  x)    ($pcall cons  x))
(define ($car   x)    ($pcall car   x))
(define ($cdr   x)    ($pcall cdr   x))
(define ($list  . x*) (let loop ((x* x*))
                        (cond ((null? x*) ($quote '()))
                              (else       ($cons (car x*) (loop (cdr x*)))))))

;;;;;;;;;;;;;;;
;;; Parsing ;;;
;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define (expression-parser parse) (identifier-qualify (fresh-identifier '||)
                                                      (list (cons vocab.expression parse))))
(define (definition-parser parse) (identifier-qualify (fresh-identifier '||)
                                                      (list (cons vocab.definition parse))))

(define (parse* env e*) (map (lambda (e) (parse env e)) e*))

(define (parse env expr)
  (let ((pv (syntax-provenance expr)) (x (syntax-unwrap expr)))
    (ast-provenance-add
      (cond
        ((identifier? expr)
         (let ((op (env-ref^ env vocab.expression expr)))
           (cond ((procedure? op)        (op env expr))
                 ((env-address env expr) (raise-syntax-error "invalid context for identifier" expr))
                 (else                   (raise-syntax-error "unbound identifier" expr)))))
        ((pair?    x) (let* ((e.op (car x))
                             (op   (and (identifier? e.op)
                                        (env-ref^ env vocab.expression:operator e.op))))
                        (if (procedure? op)
                            (op env expr)
                            (ast:call #f (parse* env (syntax->list expr))))))
        ((literal? x) (ast:quote #f x))
        (else         (raise-syntax-error "not an expression" expr)))
      pv)))

(define ((keyword-expression-parser parser argc.min argc.max) env expr)
  (let* ((e* (syntax->list expr)) (argc (- (length e*) 1)))
    (unless (<= argc.min argc)           (raise-syntax-error "too few keyword arguments"  expr))
    (unless (<= argc (or argc.max argc)) (raise-syntax-error "too many keyword arguments" expr))
    (apply parser env (cdr e*))))

(define ((parse-variable-ref/address addr)  env e) (ast:ref   (syntax-provenance e) addr))
(define ((parse-variable-quote/value value) env e) (ast:quote (syntax-provenance e) value))

(define (parse-quote        env e)           (ast:quote (syntax-provenance e) (syntax->datum e)))
(define (parse-quote-syntax env e)           (ast:quote (syntax-provenance e) e))
(define (parse-if           env e.c e.t e.f) ($if (parse env e.c) (parse env e.t) (parse env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (env:scope param* addr*)
  (let ((env.scope (make-env)))
    (unless (andmap identifier? param*)
      (raise-syntax-error "formal parameter names must be identifiers" param*))
    (unless (bound-identifiers-unique? param*)
      (raise-syntax-error "duplicate formal parameter names" param*))
    (for-each (lambda (p a)
                (env-bind! env.scope p a)
                (env-set!  env.scope vocab.expression a (parse-variable-ref/address a)))
              param* addr*)
    env.scope))

(define (env-extend-scope env param* addr*) (env-extend env (env:scope param* addr*)))

(define (parse-case-lambda env . e*.cc)
  (define (parse-case-lambda-clause e.cc)
    (let ((e* (syntax->list e.cc)))
      (when (or (null? e*) (null? (cdr e*))) (raise-syntax-error "not a case-lambda clause" e.cc))
      (let ((param*~ (syntax->improper-list (car e*))))
        (define (addr*->body . addr*)
          (parse-body (env-extend-scope env (improper-list->list param*~) addr*)
                      (cdr e*)))
        ($case-lambda-clause param*~ addr*->body))))
  (ast:case-lambda #f (map parse-case-lambda-clause e*.cc)))

(define (parse-invalid-expression env e)
  (raise-syntax-error "cannot be used in an expression context" e))

(define (env-introduce  env.scope stx.id) (car (env-introduce* env.scope (list stx.id))))
(define (env-introduce* env.scope stx*.id)
  (for-each (lambda (stx.id) (unless (identifier? stx.id)
                               (raise-syntax-error "not an identifier" stx.id)))
            stx*.id)
  (map (lambda (stx.id)
         (when (env-address env.scope stx.id)
           (raise-syntax-error "name defined multiple times" stx.id))
         (let ((addr (identifier->fresh-address stx.id)))
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
      (cons (cons addr (let ((^ast.prev (cdar dst))) (lambda () ($begin (^ast.prev) (^ast)))))
            (cdr dst))))

(define (definitions->binding-pairs defs)
  (map binding-pair (map car defs) (map (lambda (^ast) (^ast)) (map cdr defs))))

(define ((keyword-definition-parser parser argc.min argc.max) dst env.scope env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-syntax-error "too few keyword arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-syntax-error "too many keyword arguments" stx))
    (apply parser dst env.scope env (cdr stx*))))

(define (parse-body env stx.body)
  (let ((stx* (syntax->list stx.body)))
    (cond ((null? stx*)       (raise-syntax-error "no expression" stx.body))
          ((null? (cdr stx*)) (parse env (car stx*)))
          (else (let* ((env.scope (make-env))
                       (env       (env-extend env env.scope))
                       (dst       (foldl (lambda (stx dst) (parse-definition dst env.scope env stx))
                                         defstate.empty stx*)))
                  (unless (defstate-expression dst)
                    (raise-syntax-error "no expression after definitions" stx.body))
                  (ast:letrec (syntax-provenance stx.body)
                              (definitions->binding-pairs (defstate-definitions dst))
                              ((defstate-expression dst))))))))

(define (parse-definition dst env.scope env stx)
  (define (default) (defstate-add-expression dst (lambda () (parse env stx))))
  (let ((x (syntax-unwrap stx)))
    (cond ((identifier? stx) (let ((op (env-ref^ env vocab.definition stx)))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          ((pair? x)         (let* ((stx.op (car x))
                                    (op     (and (identifier? stx.op)
                                                 (env-ref^ env vocab.definition:operator stx.op))))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          (else              (default)))))

(define (parse-begin-meta dst env.scope env stx)
  (let* ((dst    (apply parse-begin-definition defstate.empty (cdr (syntax->list stx))))
         (bpair* (definitions->binding-pairs (defstate-definitions dst)))
         (addr*  (map binding-pair-lhs bpair*)))
    (for-each (lambda (addr result) (env-set! env.scope vocab.expression addr
                                              (parse-variable-quote/value result)))
              addr* (ast-eval (ast:letrec (syntax-provenance stx) bpair*
                                          ($begin ((defstate-expression dst))
                                                  (apply $list (map $ref addr*)))))))
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
                                                 (lambda (env _)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs))))))))

(define (parse-begin-definition dst env.scope env . stx*)
  (foldl (lambda (stx dst) (parse-definition dst env.scope env stx)) dst stx*))

(define (parse-begin-expression env . e*)
  (let loop ((e (car e*)) (e* (cdr e*)))
    (cond ((null? e*) (parse env e))
          (else       ($begin (parse env e) (loop (car e*) (cdr e*)))))))

(define (parse-splicing-local-definition dst env.scope env local-body . stx*)
  (let* ((stx*.local-body (syntax->list local-body))
         (env.scope.inner (make-env))
         (env             (env-extend env env.scope.inner))
         (dst             (apply parse-begin-definition dst env.scope.inner env stx*.local-body)))
    (apply parse-begin-definition dst env.scope env stx*)))

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

;; TODO: do we need these?
(define (parse-splicing-expression parse-splicing env . args)
  (parse-let env '() (definition-parser (lambda (dst env.scope env _)
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
                                (definition-parser (lambda (dst env.scope env _)
                                                     (loop dst env.scope env (cdr x)))))))))))
(define (parse-splicing-let*-expression env . args)
  (parse-splicing-expression parse-splicing-let*-definition env args))

(define (parse-and env . e*) (apply $and (parse* env e*)))
(define (parse-or  env . e*) (apply $or  (parse* env e*)))

(define (parse-when   env e.test e . e*) (apply $when   (parse* env (cons e.test (cons e e*)))))
(define (parse-unless env e.test e . e*) (apply $unless (parse* env (cons e.test (cons e e*)))))

(define (expression-keyword? kw env stx) (equal? (env-ref^ env vocab.expression:keyword stx) kw))

(define (parse-cond env clause . clause*)
  (let loop ((c* (cons clause clause*)))
    (cond ((null? c*) $void)
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (raise-syntax-error "empty clause" c))
                  (let ((e.test (car e*)) (e* (cdr e*)))
                    (cond ((expression-keyword? 'else env e.test)
                           (unless (null? e*)      (raise-syntax-error "empty else clause" c))
                           (unless (null? clause*) (raise-syntax-error "else clause is not last" c))
                           (parse-body env e*))
                          ((null? e*)
                           ($let '(test) (list (parse env e.test))
                                 (lambda (addr.test)
                                   ($if ($ref addr.test) ($ref addr.test) (loop c*)))))
                          ((expression-keyword? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (raise-syntax-error "=> is not followed by one procedure" c))
                           ($let '(test) (list (parse env e.test))
                                 (lambda (addr.test)
                                   ($if ($ref addr.test)
                                        ($call (parse env (cadr e*)) ($ref addr.test))
                                        (loop c*)))))
                          (else ($if (parse env e.test) (parse-body env e*) (loop c*))))))))))

(define (parse-binding-pairs e.bpairs)
  (define (parse-binding-pair e.bpair)
    (let ((e* (syntax->list e.bpair)))
      (unless (= (length e*) 2) (raise-syntax-error "binding pair without 2 elements" e.bpair))
      (unless (identifier? (car e*)) (raise-syntax-error "not an identifier" (car e*) e.bpair))
      (cons (car e*) (cadr e*))))
  (map parse-binding-pair (syntax->list e.bpairs)))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pairs e1)) (param* (map car bpair*)))
        (define (addr->rhs&body addr)
          (define (addr*->body . addr*)
            (parse-body (env-extend-scope env (cons e0 param*) (cons addr addr*)) e*))
          (cons (list ($lambda param* addr*->body)) ($ref addr)))
        ($call ($letrec (list e0) addr->rhs&body) (parse* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pairs e0)) (param* (map car bpair*)))
        (define (addr*->body . addr*)
          (parse-body (env-extend-scope env param* addr*) (cons e1 e*)))
        ($let param* (parse* env (map cdr bpair*)) addr*->body))))

;; TODO: instead of deep dispatching to parsers, it makes sense to switch to quasiquote-syntax with %operator embedding or transcription
;; For example, here we would use %local and %define.  parse-local itself would use %splicing-local and %let
(define (parse-letrec* env e.bpairs . e*.body)
  (let ((bpair* (parse-binding-pairs e.bpairs)))
    (apply parse-local (map (lambda (e.lhs e.rhs)
                              (definition-parser
                                (lambda (dst env.scope env _)
                                  (parse-define dst env.scope env e.lhs e.rhs))))
                            (map car bpair*) (map cdr bpair*))
           e*.body)))

(define (parse-letrec . args) (apply parse-letrec* args))

;; TODO: could define all of these via macro
(define (parse-non-splicing-expression parse-splicing env e0 e*)
  (apply parse-splicing env e0 (expression-parser (lambda (env _) (apply parse-let env '() e*)))))
(define (parse-let* env e.bpairs . e*.body)
  (parse-non-splicing-expression parse-splicing-let*-expression env e.bpairs e*.body))
(define (parse-let-syntax env e.bpairs . e*.body)
  (parse-non-splicing-expression parse-splicing-let-syntax-expression env e.bpairs e*.body))
(define (parse-local env e.defs . e*.body)
  (parse-non-splicing-expression parse-splicing-local-expression env e.defs e*.body))

;; START WITH THESE INSTEAD OF NON-SPLICING?
;; - then define non-splicing in terms of splicing

;; TODO: derive from splicing-local
;(define (parse-splicing-letrec*-expression env . args)
  ;)

;; TODO: same as splicing-letrec*
;splicing-letrec

;; TODO: derive from splicing-local
;(define (parse-splicing-letrec-syntax-expression env . args)
  ;)

;(define (parse-letrec-syntax env e.bpairs . e*.body)
  ;(let ((bpair* (parse-binding-pairs e.bpairs)))
    ;(apply parse-local (map (lambda (e.lhs e.rhs)
                              ;(definition-parser
                                ;(lambda (dst env.scope env _)
                                  ;(parse-define-syntax dst env.scope env e.lhs e.rhs))))
                            ;(map car bpair*) (map cdr bpair*))
           ;e*.body)))

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
             (unless addr (raise-syntax-error "unbound identifier" e.lhs))
             (env-set! env.scope vocab addr parser)))
          (else (let ((x (syntax-unwrap e.lhs)))
                  (cond ((pair? x) (loop (car x)
                                         (list (expression-parser
                                                 (lambda (env _)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
  dst)


(define (transcribe-and-parse-expression env.use env.op op stx)
  (parse env.use (transcribe env.op op env.use stx)))

(define (transcribe-and-parse-definition dst env.scope env.use env.op op stx)
  (parse-definition dst env.scope env.use (transcribe env.op op env.use stx)))

;; TODO: do without this (and anything else using ast-eval) until late stage bootstrapping
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
                                                 (lambda (env _)
                                                   (ast-provenance-add
                                                     (apply parse-lambda env (cdr x) e*.rhs)
                                                     (syntax-provenance (cdr x))))))))
                        (else      (raise-syntax-error "not a definable form" e.lhs)))))))
  dst)

(define initial.expression
  (list
    'quote           (keyword-expression-parser parse-quote         1 1)
    'quote-syntax    (keyword-expression-parser parse-quote-syntax  1 1)
    'if              (keyword-expression-parser parse-if            3 3)
    'case-lambda     (keyword-expression-parser parse-case-lambda   0 #f)
    'lambda          (keyword-expression-parser parse-lambda        2 #f)
    'let             (keyword-expression-parser parse-let           2 #f)

    ;; macro
    ;'letrec*         (keyword-expression-parser parse-letrec*       2 #f)
    ;'local           (keyword-expression-parser parse-local         2 #f)
    ;'let*            (keyword-expression-parser parse-let*          2 #f)
    ;'letrec          (keyword-expression-parser parse-letrec        2 #f)
    ;'letrec-syntax   (keyword-expression-parser parse-letrec-syntax 2 #f)
    ;'let-syntax      (keyword-expression-parser parse-let-syntax    2 #f)

    ;'syntax          (keyword-expression-parser parse-syntax        1 1)

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
    ;etc.
    ))

(define initial.definition
  (list
    'begin-meta     parse-begin-meta
    'define         (keyword-definition-parser parse-define         2 #f)
    'introduce      (keyword-definition-parser parse-introduce      0 #f)
    ;'declare-alias  (keyword-definition-parser parse-declare-alias  2 #f)
    'declare-parser (keyword-definition-parser parse-declare-parser 3 #f)

    ;; parser

    ;'define-syntax  (keyword-definition-parser parse-define-syntax  2 #f)

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

    ;; macro

    ;'splicing-let
    ;(keyword-definition-parser parse-splicing-let-definition        2 #f)
    ;(keyword-expression-parser parse-splicing-let-expression        2 #f)
    ;'splicing-let-syntax
    ;(keyword-definition-parser parse-splicing-let-syntax-definition 2 #f)
    ;(keyword-expression-parser parse-splicing-let-syntax-expression 2 #f)


    ;'splicing-let*
    ;'splicing-letrec
    ;'splicing-letrec*

    ;'splicing-letrec-syntax
    ))
