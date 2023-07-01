;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;

(define vocab.definition           'definition)
(define vocab.definition-operator  'definition-operator)
(define vocab.expression           'expression)
(define vocab.expression-operator  'expression-operator)
(define vocab.expression-auxiliary 'expression-auxiliary)
(define vocab.quasiquote           'quasiquote)

(define vocab-dict.empty '())
(define (vocab-dict-ref    vocab=>x vocab)    (let ((vx (assq vocab vocab=>x))) (and vx (cdr vx))))
(define (vocab-dict-remove vocab=>x . vocab*) (vocab-dict-remove* vocab=>x vocab*))

(define (vocab-dict-remove* vocab=>x vocab*)
  (filter-not (lambda (vx) (memq (car vx) vocab*)) vocab=>x))

(define (vocab-dict-set vocab=>x . vx*)
  (let loop ((vx* vx*) (vocab* '()) (x* '()))
    (cond ((null? vx*) (vocab-dict-set* vocab=>x vocab* x*))
          (else        (loop (cddr vx*) (cons (car vx*) vocab*) (cons (cadr vx*) x*))))))

(define (vocab-dict-set* vocab=>x vocab* x*)
  (foldl (lambda (vocab x vocab=>x) (if x (cons (cons vocab x) vocab=>x) vocab=>x))
         (vocab-dict-remove* vocab=>x vocab*) vocab* x*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (env-bind! env id . vx*) (env-set! env id (apply vocab-dict-set vocab-dict.empty vx*)))
(define (env-set^! env id . vx*) (let ((vocab=>v (env-ref env id)))
                                   (unless vocab=>v (error "unbound identifier" id))
                                   (env-set! env id (apply vocab-dict-set vocab=>v vx*))))
(define (env-ref^  env id vocab) (let ((vocab=>v (env-ref env id)))
                                   (and vocab=>v (vocab-dict-ref vocab=>v vocab))))

(define (env-extend env param* E*)
  (parse-param* param*)
  (env-compose env (let ((env.scope (make-env)))
                     (for-each (lambda (id E) (env-bind! env.scope id vocab.expression
                                                         (parse/constant-expression E)))
                               param* E*)
                     (env-freeze! env.scope)
                     env.scope)))

(define (env-introduce! env.scope stx.id) (env-introduce*! env.scope (list stx.id)))

(define (env-introduce*! env.scope stx*.id)
  (for-each (lambda (stx.id)
              (parse-undefined-identifier env.scope stx.id)
              (env-bind! env.scope stx.id))
            stx*.id))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-identifier id) (unless (identifier? id) (error "not an identifier" id)))

(define (parse-undefined-identifier env id)
  (parse-identifier id)
  (when (env-ref env id) (error "name defined multiple times" id)))

(define (parse-param* param*)
  (for-each parse-identifier param*)
  (let loop ((id* param*))
    (unless (null? id*)
      (let ((id.0 (car id*)))
        (when (memp (lambda (id) (bound-identifier=? id id.0)) (cdr id*))
          (error "duplicate parameter name" id.0 param*))
        (loop (cdr id*))))))

(define (parse-binding-pair* e.bpairs)
  (define (parse-binding-pair e.bpair)
    (let ((e* (syntax->list e.bpair)))
      (unless (= (length e*) 2) (error "binding pair without 2 elements" e.bpair))
      (cons (car e*) (cadr e*))))
  (map parse-binding-pair (syntax->list e.bpairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transcribe env.op op m env stx)
  (let ((result (op (syntax-add-mark stx antimark))))
    (syntax-add-mark
      (syntax-provenance-add
        (if (procedure? result)
            (let* ((lookup    (lambda (vocab id) (env-ref^ env (syntax-add-mark id m) vocab)))
                   (free-id=? (lambda (a b) (free-identifier=?/env
                                              env (syntax-add-mark a m) (syntax-add-mark b m)))))
              (result lookup free-id=?))
            result)
        (syntax-provenance stx))
      m)))

(define (transcribe-and-parse-expression env.use env.op op stx)
  (let* ((m   (fresh-mark))
         (env (env-compose env.use (env-mark env.op m))))
    (parse-expression env (transcribe env.op op m env stx))))

(define (transcribe-and-parse-definition dst env.scope.use env.use env.op op stx)
  (let* ((m            (fresh-mark))
         (env.scope.op (make-env))
         (env.scope    (env-compose env.scope.use (env-mark env.scope.op m)))
         (env          (env-compose env.use       (env-mark (env-compose env.op env.scope.op) m)))
         (dst          (parse-definition dst env.scope env (transcribe env.op op m env stx))))
    (env-freeze! env.scope.op)
    dst))

(define (identifier->fresh-address p) (fresh-address (syntax-peek p)))

(define ($provenance ast stx) (ast-provenance-add ast (syntax-provenance stx)))

(define ($quote value)       (ast:quote #f value))
(define ($ref   addr)        (ast:ref   #f addr))
(define ($call  proc . args) (ast:call  #f proc args))
(define ($if    c t f)       (ast:if    #f c t f))

(define ($case-lambda . cc*)
  (define ($case-lambda-clause param*~ arg*->body)
    (let* ((addr*~ (improper-list-map identifier->fresh-address param*~)))
      (case-lambda-clause addr*~ (apply arg*->body (map $ref (improper-list->list addr*~))))))
  (ast:case-lambda #f (map (lambda (cc) ($case-lambda-clause (car cc) (cdr cc))) cc*)))
(define ($case-lambda/env env . cc*)
  (define (convert cc)
    (let ((param*~ (car cc)) (env->body (cdr cc)))
      (cons param*~ (lambda arg* (env->body (env-extend env (improper-list->list param*~) arg*))))))
  (apply $case-lambda (map convert cc*)))

(define ($lambda         param*~     ^body) ($case-lambda (cons param*~ ^body)))
(define ($lambda/env env param*~     ^body) ($case-lambda/env env (cons param*~ ^body)))
(define ($let            param* rhs* ^body) (apply $call ($lambda param* ^body) rhs*))
(define ($let/env    env param* rhs* ^body) (apply $call ($lambda/env env param* ^body) rhs*))
(define ($letrec param* ^rhs*&body)
  (let ((addr* (map identifier->fresh-address param*)))
    (let-values ((($rhs* $body) (apply ^rhs*&body (map $ref addr*))))
      (ast:letrec #f addr* $rhs* $body))))
(define ($letrec/env env param* ^rhs*&body)
  ($letrec param* (lambda arg* (^rhs*&body (env-extend env param* arg*)))))
(define ($loop name ^rhs) ($letrec (list name) (lambda ($self) (values (list (^rhs $self)) $self))))
(define ($thunk  body) ($lambda '() (lambda ()  body)))
(define ($thunk* body) ($lambda #f  (lambda (_) body)))

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
                      (else ($let '(t ^rest)
                                  (list a ($thunk (loop (car a*) (cdr a*))))
                                  (lambda ($t $^rest) ($if $t $t ($call $^rest))))))))))

(define ($not  x) ($if x ($quote #f) ($quote #t)))

(define ($when   c body . body*) ($if c (apply $begin body body*) ($values)))
(define ($unless c body . body*) (apply $when ($not c) body body*))

(define ($prim  name)        (ast:prim #f name))
(define ($pcall name . args) (apply $call ($prim name) args))

(define ($begin a . a*) (foldl (lambda (a1 a0) ($call-with-values ($thunk a0) ($thunk* a1))) a a*))

(define $void                    ($pcall 'values))
(define ($eq?              a b)  ($pcall 'eq?     a b))
(define ($eqv?             a b)  ($pcall 'eqv?    a b))
(define ($null?            x)    ($pcall 'null?   x))
(define ($pair?            x)    ($pcall 'pair?   x))
(define ($vector?          x)    ($pcall 'vector? x))
(define ($<                a b)  ($pcall '<       a b))
(define ($+                a b)  ($pcall '+       a b))
(define ($-                a b)  ($pcall '-       a b))
(define ($cons             a b)  ($pcall 'cons    a b))
(define ($car              x)    ($pcall 'car     x))
(define ($cdr              x)    ($pcall 'cdr     x))
(define ($vector-ref       v i)  ($pcall 'vector-ref    v i))
(define ($vector-length    v)    ($pcall 'vector-length v))
(define ($vector           . x*) (apply $pcall 'vector x*))
(define ($values           . x*) (apply $pcall 'values x*))
(define ($call-with-values . x*) (apply $pcall 'call-with-values x*))
(define ($panic            . x*) (apply $pcall 'panic x*))
(define ($list             . x*) (let loop ((x* x*))
                                   (cond ((null? x*) ($quote '()))
                                         (else       ($cons (car x*) (loop (cdr x*)))))))
(define $improper-length.value
  ($lambda '(x*) (lambda ($x*)
                   ($call ($loop '(self)
                                 (lambda ($self)
                                   ($lambda '(x* acc)
                                            (lambda ($x* $acc)
                                              ($if ($pair? $x*)
                                                   ($call $self ($cdr $x*) ($+ $acc ($quote 1)))
                                                   $acc)))))
                          $x* ($quote 0)))))
(define ($improper-length x*) ($call $improper-length.value x*))
(define $length.value
  ($lambda '(x*) (lambda ($x*)
                   ($call ($loop '(self)
                                 (lambda ($self)
                                   ($lambda '(x* acc)
                                            (lambda ($x* $acc)
                                              ($if ($null? $x*)
                                                   $acc
                                                   ($call $self ($cdr $x*) ($+ $acc ($quote 1))))))))
                          $x* ($quote 0)))))
(define ($length x*) ($call $length.value x*))
(define $append.value
  ($lambda '(x* y)
           (lambda ($x* $y)
             ($call ($loop '(self)
                           (lambda ($self)
                             ($lambda '(x*) (lambda ($x*)
                                              ($if ($null? $x*)
                                                   $y
                                                   ($cons ($car $x*) ($call $self ($cdr $x*))))))))
                    $x*))))
(define ($append x* y) ($call $append.value x* y))
;; TODO: $vector->list.value and $vector->list

(define defstate.empty '())

(define (defstate-entry addr ^ast assign!) (vector addr ^ast assign!))
(define (defstate-entry-address  entry)    (vector-ref entry 0))
(define (defstate-entry-^ast     entry)    (vector-ref entry 1))
(define (defstate-entry-assigner entry)    (vector-ref entry 2))

(define (defstate-definition* dst) (reverse (if (defstate-entry-address (car dst)) dst (cdr dst))))
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
(define (defstate-set-expression dst ^ast) (defstate-add-expression
                                             (if (defstate-expression dst) (cdr dst) dst) ^ast))

(define (definition*->address*  def*) (map defstate-entry-address def*))
(define (definition*->ast*      def*) (map (lambda (^ast) (^ast)) (map defstate-entry-^ast def*)))
(define (definition*->assigner* def*) (map defstate-entry-assigner def*))

(define ($define dst env.scope lhs ^rhs)
  (env-introduce! env.scope lhs)
  (let* ((addr    (identifier->fresh-address lhs))
         (parser  (mvector (parse/constant-expression ($ref addr))))
         (assign! (lambda (value)
                    (mvector-set! parser 0 (parse/constant-expression ($quote value))))))
    (env-set^! env.scope lhs vocab.expression (lambda arg* (apply (mvector-ref parser 0) arg*)))
    (defstate-define/assign! dst addr ^rhs assign!)))

(define (defstate->ast dst)
  (let ((def* (defstate-definition* dst)))
    ;; TODO: redo defstates in terms of identifiers instead of addresses so that we can eliminate
    ;; this ast: construction.
    (ast:letrec #f (definition*->address* def*) (definition*->ast* def*)
                ((or (defstate-expression dst) $values)))))

(define ((defstate->ast/eval ast-eval) dst)
  (let* ((addr*    (definition*->address* (defstate-definition* dst)))
         (ast.^e   ($thunk ((or (defstate-expression dst) $values))))
         (dst      (defstate-set-expression dst (lambda () (apply $list ast.^e (map $ref addr*)))))
         (assign!* (definition*->assigner* (defstate-definition* dst))))
    (let ((result* (ast-eval (defstate->ast dst))))
      (for-each (lambda (assign! result) (assign! result)) assign!* (cdr result*))
      (call-with-values (car result*) (lambda x* (apply $values (map $quote x*)))))))

(define ($body env ^def)
  (let* ((env.scope (make-env))
         (dst       (^def defstate.empty env.scope (env-compose env env.scope))))
    (env-freeze! env.scope)
    (defstate->ast dst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define ((auxiliary?/vocab vocab) a env stx.id)
  (and (identifier? stx.id) (equal? (env-ref^ env stx.id vocab) a)))
(define expression-auxiliary? (auxiliary?/vocab vocab.expression-auxiliary))

(define (parse-expression* env e*) (map (lambda (e) (parse-expression env e)) e*))

(define (parse-expression env expr)
  (let ((x (syntax-unwrap expr)))
    ($provenance
      (cond
        ((identifier? expr)
         (let ((op (env-ref^ env expr vocab.expression)))
           (cond ((procedure? op)    (op env expr))
                 ((env-ref env expr) (error "non-expression identifier" expr))
                 (else               (error "unbound identifier" expr)))))
        ((pair?    x) (let* ((e.op (car x))
                             (op   (and (identifier? e.op)
                                        (env-ref^ env e.op vocab.expression-operator))))
                        (if (procedure? op)
                            (op env expr)
                            (apply $call (parse-expression env e.op)
                                   (parse-expression* env (syntax->list (cdr x)))))))
        ((literal? x) ($quote x))
        (else         (error "not an expression" expr)))
      expr)))

(define ((expression-operator-parser parser argc.min argc.max) env expr)
  (let* ((e* (syntax->list expr)) (argc (- (length e*) 1)))
    (unless (<= argc.min argc)           (error "too few operator arguments"  expr))
    (unless (<= argc (or argc.max argc)) (error "too many operator arguments" expr))
    (apply parser env (cdr e*))))

(define ((parse/constant-expression E) env _) E)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-definition dst env.scope env stx)
  (define (default) (defstate-add-expression dst (lambda () (parse-expression env stx))))
  (let ((x (syntax-unwrap stx)))
    (cond ((identifier? stx) (let ((op (env-ref^ env stx vocab.definition)))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          ((pair? x)         (let* ((stx.op (car x))
                                    (op     (and (identifier? stx.op)
                                                 (env-ref^ env stx.op vocab.definition-operator))))
                               (if (procedure? op)
                                   (op dst env.scope env stx)
                                   (default))))
          (else              (default)))))

(define ((definition-operator-parser parser argc.min argc.max) dst env.scope env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (error "too many operator arguments" stx))
    (apply parser dst env.scope env (cdr stx*))))
