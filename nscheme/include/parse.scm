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

(define (env-introduce! env stx.id) (env-introduce*! env (list stx.id)))

(define (env-introduce*! env stx*.id)
  (for-each (lambda (stx.id)
              (parse-undefined-identifier env stx.id)
              (env-bind! env stx.id))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax transformation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (transcribe-and-parse-definition env.d.use env.use env.op op stx)
  (let* ((m        (fresh-mark))
         (env.d.op (make-env))
         (env.d    (env-compose env.d.use (env-mark env.d.op m)))
         (env      (env-compose env.use   (env-mark (env-compose env.op env.d.op) m)))
         (D        (parse-definition env.d env (transcribe env.op op m env stx))))
    ;; TODO: defer this freeze until after the entire definition body has been processed.
    ;(env-freeze! env.d.op)
    D))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ($provenance/syntax stx E) ($provenance (syntax-provenance stx) E))
(define ($case-lambda/env env . cc*)
  (define (convert cc)
    (let ((param*~ (car cc)) (env->body (cdr cc)))
      (cons param*~ (lambda arg* (env->body (env-extend env (improper-list->list param*~) arg*))))))
  (apply $case-lambda (map convert cc*)))
(define ($lambda         param*~     ^body) ($case-lambda (cons param*~ ^body)))
(define ($lambda/env env param*~     ^body) ($case-lambda/env env (cons param*~ ^body)))
(define ($let            param* rhs* ^body) (apply $call ($lambda param* ^body) rhs*))
(define ($let/env    env param* rhs* ^body) (apply $call ($lambda/env env param* ^body) rhs*))
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
(define ($begin a . a*) (foldl (lambda (a1 a0) ($call-with-values ($thunk a0) ($thunk* a1))) a a*))
(define ($not                 x) ($if x ($quote #f) ($quote #t)))
(define ($when   c body . body*) ($if c (apply $begin body body*) ($values)))
(define ($unless c body . body*) (apply $when ($not c) body body*))
(define ($pcall     name . args) (apply $call ($prim name) args))
(define $void                    ($pcall 'values))
(define ($eq?               a b) ($pcall 'eq?     a b))
(define ($eqv?              a b) ($pcall 'eqv?    a b))
(define ($null?             x)   ($pcall 'null?   x))
(define ($pair?             x)   ($pcall 'pair?   x))
(define ($vector?           x)   ($pcall 'vector? x))
(define ($<                 a b) ($pcall '<       a b))
(define ($+                 a b) ($pcall '+       a b))
(define ($-                 a b) ($pcall '-       a b))
(define ($cons              a b) ($pcall 'cons    a b))
(define ($car               x)   ($pcall 'car     x))
(define ($cdr               x)   ($pcall 'cdr     x))
(define ($vector-ref        v i) ($pcall 'vector-ref    v i))
(define ($vector-length     v)   ($pcall 'vector-length v))
(define ($vector           . x*) (apply $pcall 'vector x*))
(define ($values           . x*) (apply $pcall 'values x*))
(define ($call-with-values . x*) (apply $pcall 'call-with-values x*))
(define ($quote-values     . x*) (apply $values (map $quote x*)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition contexts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define defstate.empty '())
(define (defstate-entry id env ^E)  (vector id env ^E))
(define (defstate-entry-id   entry) (vector-ref entry 0))
(define (defstate-entry-env  entry) (vector-ref entry 1))
(define (defstate-entry-^rhs entry) (vector-ref entry 2))
(define (defstate-definition* dst)  (if (null? dst)
                                        '()
                                        (reverse (if (defstate-entry-id (car dst)) dst (cdr dst)))))
(define (defstate-expression  dst)  (and (pair? dst) (not (defstate-entry-id (car dst)))
                                         (defstate-entry-^rhs (car dst))))
(define (defstate-define dst id env ^E)
  (if (or (null? dst) (defstate-entry-id (car dst)))
      (cons (defstate-entry id env ^E) dst)
      (let ((^E (let ((^E.prev (defstate-entry-^rhs (car dst))))
                  (lambda () ($begin (^E.prev) (^E))))))
        (cons (defstate-entry id env ^E) (cdr dst)))))
(define (defstate-replace-expression dst ^E)
  (defstate-define (if (defstate-expression dst) (cdr dst) dst) #f #f ^E))

(define (definition*->id*  def*) (map defstate-entry-id  def*))
(define (definition*->env* def*) (map defstate-entry-env def*))
(define (definition*->rhs* def*) (map (lambda (^E) (^E)) (map defstate-entry-^rhs def*)))

(splicing-local
  ((define (env-set-variable! env id E) (env-set^! env id vocab.expression
                                                   (parse/constant-expression E))))
  (define (defstate->E dst)
    (let* ((def* (defstate-definition* dst))
           (id*  (definition*->id* def*)))
      ($letrec id* (lambda E*
                     (for-each env-set-variable! (definition*->env* def*) id* E*)
                     (values (definition*->rhs* def*) ((or (defstate-expression dst) $values)))))))
  (define ((defstate->E/eval E-eval) dst)
    (let* ((def* (defstate-definition* dst))
           (id*  (definition*->id*  def*))
           (env* (definition*->env* def*))
           (E.^e ($thunk ((or (defstate-expression dst) $values))))
           (dst  (defstate-replace-expression
                   dst (lambda () (apply $list E.^e (map parse-expression env* id*))))))
      (let ((result* (E-eval (defstate->E dst))))
        (for-each env-set-variable! env* id* (map $quote (cdr result*)))
        (call-with-values (car result*) $quote-values)))))

(define (D:annotated         pv D) (vector 'D:annotated  pv D))
(define (D:begin               D*) (vector 'D:begin      D*))
(define (D:definition id env ^rhs) (vector 'D:definition id env ^rhs))
(define (D:expression          ^E) (vector 'D:expression ^E))

(define (D-tag             D) (vector-ref D 0))
(define (D:annotated-pv    D) (vector-ref D 1))
(define (D:annotated-D     D) (vector-ref D 2))
(define (D:begin-D*        D) (vector-ref D 1))
(define (D:definition-id   D) (vector-ref D 1))
(define (D:definition-env  D) (vector-ref D 2))
(define (D:definition-^rhs D) (vector-ref D 3))
(define (D:expression-^E   D) (vector-ref D 1))

(define (D-tagged? D tag) (eq? (D-tag D) tag))
(define (D-provenance D) (and (D-tagged? D 'D:annotated) (D:annotated-pv D)))

(define (D->defstate D)
  (let loop ((D D) (dst defstate.empty))
    (cond ((D-tagged? D 'D:begin)      (foldl loop dst (D:begin-D* D)))
          ((D-tagged? D 'D:definition) (defstate-define dst (D:definition-id D) (D:definition-env D)
                                                        (D:definition-^rhs D)))
          ((D-tagged? D 'D:expression) (defstate-define dst #f #f (D:expression-^E D)))
          ((D-tagged? D 'D:annotated)  (loop (D:annotated-D D) dst))
          (else                        (error "invalid definition" D)))))
(define (D->E D) (defstate->E (D->defstate D)))

(define ($d:provenance       pv D) (D:annotated pv D))
(define ($d:begin            . D*) (D:begin D*))
(define ($d:expression         ^E) (D:expression ^E))
(define ($d:define env.d lhs ^rhs) (env-introduce! env.d lhs) (D:definition lhs env.d ^rhs))

(define ($body env ^def)
  (let* ((env.d (make-env))
         (D     (^def env.d (env-compose env env.d)))
         (dst   (D->defstate D)))
    (unless (defstate-expression dst)
      (error
        (if (null? (defstate-definition* dst)) "no expression" "no expression after definitions")
        (D-provenance D)))
    (let ((E (defstate->E dst)))
      (env-freeze! env.d)
      E)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define ((auxiliary?/vocab vocab) a env stx.id)
  (and (identifier? stx.id) (equal? (env-ref^ env stx.id vocab) a)))
(define expression-auxiliary? (auxiliary?/vocab vocab.expression-auxiliary))

(define (parse-expression* env stx*) (map (lambda (stx) (parse-expression env stx)) stx*))

(define (parse-expression env stx)
  (let ((x (syntax-unwrap stx)))
    ($provenance/syntax
      stx
      (cond
        ((identifier? stx)
         (let ((op (env-ref^ env stx vocab.expression)))
           (cond ((procedure? op)   (op env stx))
                 ((env-ref env stx) (error "non-expression or out-of-phase identifier" stx))
                 (else              (error "unbound identifier" stx)))))
        ((pair?    x) (let* ((e.op (car x))
                             (op   (and (identifier? e.op)
                                        (env-ref^ env e.op vocab.expression-operator))))
                        (if (procedure? op)
                            (op env stx)
                            (apply $call (parse-expression env e.op)
                                   (parse-expression* env (syntax->list (cdr x)))))))
        ((literal? x) ($quote x))
        (else         (error "not an expression" stx))))))

(define ((expression-operator-parser parser argc.min argc.max) env expr)
  (let* ((e* (syntax->list expr)) (argc (- (length e*) 1)))
    (unless (<= argc.min argc)           (error "too few operator arguments"  expr))
    (unless (<= argc (or argc.max argc)) (error "too many operator arguments" expr))
    (apply parser env (cdr e*))))

(define ((parse/constant-expression E) env _) E)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-definition env.d env stx)
  (define (default) ($d:expression (lambda () (parse-expression env stx))))
  (let ((x (syntax-unwrap stx)))
    (cond ((identifier? stx) (let ((op (env-ref^ env stx vocab.definition)))
                               (if (procedure? op)
                                   (op env.d env stx)
                                   (default))))
          ((pair? x)         (let* ((stx.op (car x))
                                    (op     (and (identifier? stx.op)
                                                 (env-ref^ env stx.op vocab.definition-operator))))
                               (if (procedure? op)
                                   (op env.d env stx)
                                   (default))))
          (else              (default)))))

(define ((definition-operator-parser parser argc.min argc.max) env.d env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (error "too many operator arguments" stx))
    (apply parser env.d env (cdr stx*))))
