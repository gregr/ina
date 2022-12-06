;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;

(define vocab.definition           'definition)
(define vocab.definition-operator  'definition-operator)
(define vocab.expression           'expression)
(define vocab.expression-operator  'expression-operator)
(define vocab.expression-auxiliary 'expression-auxiliary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identifier->fresh-address p) (fresh-address (identifier-unqualify p)))

(define ($quote value)       (ast:quote #f value))
(define ($ref   addr)        (ast:ref   #f addr))
(define ($call  proc . args) (ast:call  #f proc args))
(define ($if    c t f)       (ast:if #f c t f))

(define (ast:lambda pv param   body) (ast:case-lambda pv (list (case-lambda-clause param body))))
(define (ast:let    pv p* rhs* body) (ast:call        pv (ast:lambda #f p* body) rhs*))

(define ($case-lambda-clause env param*~ env&addr*->body)
  (let* ((addr*~ (improper-list-map identifier->fresh-address param*~))
         (addr*  (improper-list->list addr*~))
         (env    (env-extend-scope env (improper-list->list param*~) addr*)))
    (case-lambda-clause addr*~ (apply env&addr*->body env addr*))))

(define ($case-lambda env . cc*)
  (ast:case-lambda #f (map (lambda (cc) ($case-lambda-clause env (car cc) (cdr cc))) cc*)))

(define ($lambda env param*~     ^body) ($case-lambda env (cons param*~ ^body)))
(define ($let    env param* rhs* ^body) (apply $call ($lambda env param* ^body) rhs*))

(define ($letrec env param* env&addr*->rhs*&body)
  (let* ((addr*     (map identifier->fresh-address param*))
         (env       (env-extend-scope env param* addr*)))
    (let-values ((($rhs* $body) (apply env&addr*->rhs*&body env addr*)))
      (ast:letrec #f (map binding-pair addr* $rhs*) $body))))

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
                      (else (ast:let #f '(t ^rest)
                                     (list a (ast:lambda #f '() (loop (car a*) (cdr a*))))
                                     ($if ($ref 't) ($ref 't) ($call ($ref '^rest))))))))))

(define ($not  x) ($if x ($quote #f) ($quote #t)))

(define ($when   c body . body*) ($if c (apply $begin body body*) ($call ($quote values))))
(define ($unless c body . body*) (apply $when ($not c) body body*))

(define ($pcall prim . args) (apply $call ($quote prim) args))

(define ($begin  a . a*)
  (foldr (lambda (a1 a0) ($pcall call-with-values (ast:lambda #f '() a0) (ast:lambda #f #f  a1)))
         a a*))

(define $void         ($pcall values))
(define ($null? x)    ($pcall null? x))
(define ($pair? x)    ($pcall pair? x))
(define ($cons  a b)  ($pcall cons  a b))
(define ($car   x)    ($pcall car   x))
(define ($cdr   x)    ($pcall cdr   x))
(define ($list  . x*) (let loop ((x* x*))
                        (cond ((null? x*) ($quote '()))
                              (else       ($cons (car x*) (loop (cdr x*)))))))

(define defstate.empty '())

(define (defstate-entry addr ^ast assign!) (vector addr ^ast assign!))
(define (defstate-entry-address  entry)    (vector-ref entry 0))
(define (defstate-entry-^ast     entry)    (vector-ref entry 1))
(define (defstate-entry-assigner entry)    (vector-ref entry 2))

(define (defstate-definitions dst) (reverse (if (defstate-entry-address (car dst)) dst (cdr dst))))
(define (defstate-expression  dst) (and (not (defstate-entry-address (car dst)))
                                        (defstate-entry-^ast (car dst))))

(define (defstate-define/assign! dst addr ^ast assign!)
  (if (or (null? dst) (defstate-entry-address (car dst)))
      (cons (defstate-entry addr ^ast assign!) dst)
      (let ((^ast (let ((^ast.prev (defstate-entry-^ast (car dst))))
                    (lambda () ($begin (^ast.prev) (^ast))))))
        (cons (defstate-entry addr ^ast assign!) (cdr dst)))))

(define (defstate-define dst addr ^ast)
  (defstate-define/assign! dst addr ^ast (lambda (v) (values))))

(define (defstate-add-expression dst ^ast) (defstate-define dst #f ^ast))

(define (definitions->binding-pairs def*)
  (map binding-pair
       (map defstate-entry-address def*)
       (map (lambda (^ast) (^ast)) (map defstate-entry-^ast def*))))

(define (definitions->assigners def*) (map defstate-entry-assigner def*))

(define ($define dst env.scope lhs ^rhs)
  (let* ((addr    (env-introduce env.scope lhs))
         (parser  (parse-variable-ref/address addr))
         (assign! (lambda (value) (set! parser (parse-variable-quote/value value)))))
    (env-set! env.scope vocab.expression addr (lambda arg* (apply parser arg*)))
    (defstate-define/assign! dst addr ^rhs assign!)))

(define ($body env ^def)
  (let* ((env.scope (make-env))
         (env       (env-extend env env.scope))
         (dst       (^def defstate.empty env.scope env)))
    (ast:letrec #f (definitions->binding-pairs (defstate-definitions dst))
                ((defstate-expression dst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (env:scope param* addr*)
  (let ((env.scope (make-env)))
    (unless (andmap identifier? param*)
      (raise-syntax-error "parameter names must be identifiers" param*))
    (unless (bound-identifiers-unique? param*)
      (raise-syntax-error "duplicate parameter names" param*))
    (for-each (lambda (p a)
                (env-bind! env.scope p a)
                (env-set!  env.scope vocab.expression a (parse-variable-ref/address a)))
              param* addr*)
    env.scope))

(define (env-extend-scope env param* addr*) (env-extend env (env:scope param* addr*)))

(define (transcribe-and-parse-expression env.use env.op op stx)
  (parse-expression env.use (transcribe env.op op env.use stx)))

(define (expression-auxiliary? a env stx) (equal? (env-ref^ env vocab.expression-auxiliary stx) a))

(define (parse-expression* env e*) (map (lambda (e) (parse-expression env e)) e*))

(define (parse-expression env expr)
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
                            (ast:call #f (parse-expression* env (syntax->list expr))))))
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

(define (parse-definition dst env.scope env stx)
  (define (default) (defstate-add-expression dst (lambda () (parse-expression env stx))))
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
