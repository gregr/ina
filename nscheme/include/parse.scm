;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;

(define vocab.definition          'definition)
(define vocab.definition-operator 'definition-operator)
(define vocab.expression          'expression)
(define vocab.expression-operator 'expression-operator)
(define vocab.expression-keyword  'expression-keyword)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (transcribe-and-parse-expression env.use env.op op stx)
  (parse env.use (transcribe env.op op env.use stx)))

(define (expression-keyword? kw env stx) (equal? (env-ref^ env vocab.expression-keyword stx) kw))

(define (expression-parser parse) (identifier-qualify (fresh-identifier '||)
                                                      (list (cons vocab.expression parse))))

(define (parse* env e*) (map (lambda (e) (parse env e)) e*))

(define (parse env expr)
  (define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))
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
                                        (env-ref^ env vocab.expression-operator e.op))))
                        (if (procedure? op)
                            (op env expr)
                            (ast:call #f (parse* env (syntax->list expr))))))
        ((literal? x) (ast:quote #f x))
        (else         (raise-syntax-error "not an expression" expr)))
      pv)))

(define ((expression-operator-parser parser argc.min argc.max) env expr)
  (let* ((e* (syntax->list expr)) (argc (- (length e*) 1)))
    (unless (<= argc.min argc)           (raise-syntax-error "too few operator arguments"  expr))
    (unless (<= argc (or argc.max argc)) (raise-syntax-error "too many operator arguments" expr))
    (apply parser env (cdr e*))))

(define ((parse-variable-ref/address addr)  env e) (ast:ref   (syntax-provenance e) addr))
(define ((parse-variable-quote/value value) env e) (ast:quote (syntax-provenance e) value))

(define (parse-invalid-expression env e)
  (raise-syntax-error "cannot be used in an expression context" e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (transcribe-and-parse-definition dst env.scope env.use env.op op stx)
  (parse-definition dst env.scope env.use (transcribe env.op op env.use stx)))

(define (definition-parser parse) (identifier-qualify (fresh-identifier '||)
                                                      (list (cons vocab.definition parse))))

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

(define (parse-definition dst env.scope env stx)
  (define (default) (defstate-add-expression dst (lambda () (parse env stx))))
  (let ((x (syntax-unwrap stx)))
    (cond ((identifier? stx) (let ((op (env-ref^ env vocab.definition stx)))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          ((pair? x)         (let* ((stx.op (car x))
                                    (op     (and (identifier? stx.op)
                                                 (env-ref^ env vocab.definition-operator stx.op))))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          (else              (default)))))

(define ((definition-operator-parser parser argc.min argc.max) dst env.scope env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-syntax-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-syntax-error "too many operator arguments" stx))
    (apply parser dst env.scope env (cdr stx*))))
