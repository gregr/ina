;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;
(define vocab.definition           'definition)
(define vocab.definition-operator  'definition-operator)
(define vocab.expression           'expression)
(define vocab.expression-operator  'expression-operator)
(define vocab.expression-auxiliary 'expression-auxiliary)
(define vocab.set!                 'set!)
(define vocab.set!-operator        'set!-operator)

(define vocab-dict.empty '())
(define (vocab-dict-ref    vocab=>x vocab)    (let ((vx (assv vocab vocab=>x))) (and vx (cdr vx))))
(define (vocab-dict-remove vocab=>x . vocab*) (vocab-dict-remove* vocab=>x vocab*))
(define (vocab-dict-remove* vocab=>x vocab*)
  (filter (lambda (vx) (not (memv (car vx) vocab*))) vocab=>x))
(define (vocab-dict-set vocab=>x . vx*) (vocab-dict-set* vocab=>x vx*))
(define (vocab-dict-set* vocab=>x vx*)
  (let loop ((vx* vx*) (vocab* '()) (x* '()))
    (cond ((null? vx*) (vocab-dict-set** vocab=>x vocab* x*))
          (else        (loop (cddr vx*) (cons (car vx*) vocab*) (cons (cadr vx*) x*))))))
(define (vocab-dict-set** vocab=>x vocab* x*)
  (fold-left (lambda (vocab=>x vocab x) (if x (cons (cons vocab x) vocab=>x) vocab=>x))
             (vocab-dict-remove* vocab=>x vocab*) vocab* x*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (env-vocabulary-bind! env id . vx*)
  (env-set! env id (vocab-dict-set* vocab-dict.empty vx*)))
(define (env-vocabulary-set! env id . vx*)
  (let ((vocab=>v (env-ref env id)))
    (unless vocab=>v (mistake 'env-vocabulary-set! "unbound identifier" id))
    (env-set! env id (vocab-dict-set* vocab=>v vx*))))
(define (env-vocabulary-ref env id vocab)
  (let ((vocab=>v (env-ref env id))) (and vocab=>v (vocab-dict-ref vocab=>v vocab))))

(define (env-extend env param* E*)
  (parse-param* param*)
  (env-conjoin (let ((env.scope (make-env)))
                 (for-each (lambda (id E) (env-vocabulary-bind! env.scope id vocab.expression
                                                                (parse/constant-expression E)))
                           param* E*)
                 (env-read-only env.scope))
               env))

(define (env-introduce! env stx.id) (env-introduce*! env (list stx.id)))

(define (env-introduce*! env stx*.id)
  (for-each (lambda (stx.id)
              (parse-undefined-identifier env stx.id)
              (env-vocabulary-bind! env stx.id))
            stx*.id))

(define (env-introduce-boxed! env id ^E.box)
  (env-introduce! env id)
  (env-vocabulary-set!
    env id
    vocab.expression (lambda (env _) ($unbox (^E.box)))
    vocab.set! (lambda (env stx.lhs E.rhs) ($set-box! ($source (^E.box) stx.lhs) E.rhs))))

(define (env-add-alist! env a)
  (alist-for-each a (lambda (id E) (env-vocabulary-bind!
                                     env id vocab.expression (parse/constant-expression E)))))
(define (alist->env a)
  (let ((env (make-env)))
    (env-add-alist! env a)
    (env-read-only env)))
(define (value-alist->env a) (alist->env (alist-map-value a $quote)))
(define (addr-alist->env  a) (alist->env (alist-map-value a $ref)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax->improper-list s)
  (let ((x (syntax-unwrap s)))
    (if (pair? x)
        (cons (car x) (syntax->improper-list (cdr x)))
        x)))

(define (syntax->list s)
  (let ((x*~ (syntax->improper-list s)))
    (unless (list? x*~) (raise-parse-error "not a syntax list" s))
    x*~))

(define (parse-identifier id) (unless (identifier? id) (raise-parse-error "not an identifier" id)))

(define (parse-undefined-identifier env id)
  (parse-identifier id)
  (when (env-ref env id) (raise-parse-error "name defined multiple times" id)))

(define (parse-param* param*)
  (for-each parse-identifier param*)
  (let loop ((id* param*))
    (unless (null? id*)
      (let ((id.0 (car id*)))
        (when (memp (lambda (id) (identifier=? id id.0)) (cdr id*))
          (raise-parse-error "duplicate parameter name" (list id.0 param*)))
        (loop (cdr id*))))))

(define (parse-binding-pair* e.bpairs)
  (define (parse-binding-pair e.bpair)
    (let ((e* (syntax->list e.bpair)))
      (unless (= (length e*) 2) (raise-parse-error "binding pair without 2 elements" e.bpair))
      (cons (car e*) (cadr e*))))
  (map parse-binding-pair (syntax->list e.bpairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax transformation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transcribe-and-parse-expression op env.op env.use stx)
  (let* ((m   (fresh-mark))
         (env (env-disjoin env.op m env.use)))
    (parse-expression env (transcribe op m env stx))))

(define (transcribe-and-parse-definition op env.op env.d.use env.use stx)
  (let* ((m        (fresh-mark))
         (env.d.op (make-env))
         (env.d    (env-disjoin env.d.op m env.d.use))
         (env      (env-disjoin (env-conjoin env.d.op env.op) m env.use)))
    (parse-definition env.d env (transcribe op m env stx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expression construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ($source E stx) ($annotate E stx))
(splicing-local
  ((define (convert-body env param*~ env->body)
     (lambda arg* (env->body (env-extend env (improper-list->list param*~) arg*)))))
  (define ($case-lambda/env env param*~* env->body*)
    ($case-lambda param*~* (map (lambda (p*~ env->b) (convert-body env p*~ env->b))
                                param*~* env->body*)))
  (define ($lambda/env env param*~     ^body) ($lambda param*~ (convert-body env param*~ ^body)))
  (define ($let/env    env param* rhs* ^body) ($let param* rhs* (convert-body env param* ^body))))
(define ($let1         param rhs ^body) ($let         (list param) (list rhs) ^body))
(define ($let1/env env param rhs ^body) ($let/env env (list param) (list rhs) ^body))
(define ($letrec/env env param* ^rhs*&body)
  ($letrec param* (lambda arg* (^rhs*&body (env-extend env param* arg*)))))
(define ($loop name ^rhs) ($letrec (list name) (lambda ($self) (values (list (^rhs $self)) $self))))
(define ($thunk body) ($lambda '() (lambda () body)))
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
(define ($not                 x) ($if x ($quote #f) ($quote #t)))
(define ($when   c body . body*) ($if c (apply $begin body body*) ($values)))
(define ($unless c body . body*) (apply $when ($not c) body body*))
(define ($pcall     prim . args) ($call* ($quote prim) args))
(define ($eqv?              a b) ($pcall eqv?    a b))
(define ($null?             x)   ($pcall null?   x))
(define ($pair?             x)   ($pcall pair?   x))
(define ($vector?           x)   ($pcall vector? x))
(define ($=                 a b) ($pcall =       a b))
(define ($<                 a b) ($pcall <       a b))
(define ($+                 a b) ($pcall +       a b))
(define ($-                 a b) ($pcall -       a b))
(define ($cons              a b) ($pcall cons    a b))
(define ($car               x)   ($pcall car     x))
(define ($cdr               x)   ($pcall cdr     x))
(define ($vector-ref        v i) ($pcall vector-ref    v i))
(define ($vector-length     v)   ($pcall vector-length v))
(define ($vector           . x*) (apply $pcall vector x*))
(define ($values           . x*) (apply $pcall values x*))
(define ($quote-values     . x*) (apply $values (map $quote x*)))
(define ($mistake     . detail*) (apply $pcall panic ($quote 'mistake) detail*))
(define ($list             . x*) (let loop ((x* x*))
                                   (cond ((null? x*) ($quote '()))
                                         (else       ($cons (car x*) (loop (cdr x*)))))))
(define ($box      x)   ($pcall make-mvector ($quote 1) x))
(define ($unbox    b)   ($pcall mvector-ref  b ($quote 0)))
(define ($set-box! b x) ($pcall mvector-set! b ($quote 0) x))

(define $improper-length.value
  ($lambda '(x*) (lambda ($x*)
                   ($call ($loop 'self
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
                   ($call ($loop 'self
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
             ($call ($loop 'self
                           (lambda ($self)
                             ($lambda '(x*) (lambda ($x*)
                                              ($if ($null? $x*)
                                                   $y
                                                   ($cons ($car $x*) ($call $self ($cdr $x*))))))))
                    $x*))))
(define ($append x* y) ($call $append.value x* y))

(define ($literal-equal? d $x)
  (let loop ((d d) ($x $x))
    (cond
      ((pair?   d) ($and ($pair? $x)
                         ($let '(x.car) (list ($car $x)) (lambda ($x) (loop (car d) $x)))
                         ($let '(x.cdr) (list ($cdr $x)) (lambda ($x) (loop (cdr d) $x)))))
      ((vector? d) (apply $and ($vector? $x)
                          ($= ($vector-length $x) ($quote (vector-length d)))
                          (map (lambda (i) ($let '(x.i) (list ($vector-ref $x ($quote i)))
                                                 (lambda ($x) (loop (vector-ref d i) $x))))
                               (range (vector-length d)))))
      (else        ($eqv? $x ($quote d))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition contexts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(splicing-local
  ((define (D:begin          D*)          (vector 'D:begin          D*))
   (define (D:definition     id env ^rhs) (vector 'D:definition     id env ^rhs))
   (define (D:expression     ^E)          (vector 'D:expression     ^E))
   (define (D:end/expression D stx)       (vector 'D:end/expression D stx))

   (define (D-tag                   D) (vector-ref D 0))
   (define (D:begin-D*              D) (vector-ref D 1))
   (define (D:definition-id         D) (vector-ref D 1))
   (define (D:definition-env        D) (vector-ref D 2))
   (define (D:definition-^rhs       D) (vector-ref D 3))
   (define (D:expression-^E         D) (vector-ref D 1))
   (define (D:end/expression-D      D) (vector-ref D 1))
   (define (D:end/expression-syntax D) (vector-ref D 2))
   (define (D-tagged? D tag) (eqv? (D-tag D) tag))

   (define (env-set-variable! env id E)
     (env-vocabulary-set! env id vocab.expression (parse/constant-expression E)))
   (define (definition id env ^E)  (vector id env ^E))
   (define (definition-id   entry) (vector-ref entry 0))
   (define (definition-env  entry) (vector-ref entry 1))
   (define (definition-^rhs entry) (vector-ref entry 2))
   (define (D-compile D publish-values?)
     (mlet ((rdef* '()) (^E.current #f))
       (let loop! ((D D))
         (cond ((D-tagged? D 'D:begin) (for-each loop! (D:begin-D* D)))
               ((D-tagged? D 'D:definition)
                (let ((^E   ^E.current)
                      (^E.D (D:definition-^rhs D)))
                  (set! rdef* (cons (definition (D:definition-id D) (D:definition-env D)
                                                (if ^E (lambda () ($begin (^E) (^E.D))) ^E.D))
                                    rdef*))
                  (set! ^E.current #f)))
               ((D-tagged? D 'D:expression)
                (let ((^E   ^E.current)
                      (^E.D (D:expression-^E D)))
                  (set! ^E.current (if ^E (lambda () ($begin (^E) (^E.D))) ^E.D))))
               ((D-tagged? D 'D:end/expression)
                (loop! (D:end/expression-D D))
                (unless ^E.current
                  (raise-parse-error
                    (if (null? rdef*) "no expression" "no expression after definitions")
                    (D:end/expression-syntax D))))
               (else (mistake 'D-compile "not a D" D))))
       (let ((def* (reverse rdef*)) (^E (or ^E.current $values)))
         (if (null? rdef*)
             (^E)
             (let* ((id*   (map definition-id   def*))
                    (env*  (map definition-env  def*))
                    (^rhs* (map definition-^rhs def*))
                    (^E    (if publish-values?
                               (lambda ()
                                 (define (publish-rhs*! rhs*)
                                   (for-each env-set-variable! env* id* (map $quote rhs*)))
                                 ($let '(result* rhs*)
                                       (list ($apply/values
                                               ($lambda 'result* (lambda ($result*) $result*))
                                               (^E))
                                             (apply $list (map parse-expression env* id*)))
                                       (lambda ($result* $rhs*)
                                         ($begin ($call ($quote publish-rhs*!) $rhs*)
                                                 ($pcall apply ($quote values) $result*)))))
                               ^E)))
               ($letrec id* (lambda E*
                              (for-each env-set-variable! env* id* E*)
                              (values (map (lambda (^rhs) (^rhs)) ^rhs*) (^E))))))))))
  (define (D->E         D) (D-compile D #f))
  (define (D->E/publish D) (D-compile D #t))
  (define ($d:begin . D*)           (D:begin D*))
  (define ($d:end/expression D stx) (D:end/expression D stx))
  (define ($d:expression ^E)        (D:expression ^E))
  (define ($d:define env.d lhs ^rhs)
    (env-introduce! env.d lhs)
    (env-vocabulary-set! env.d lhs vocab.expression
                         (lambda (env stx)
                           (raise-parse-error
                             "parsed variable reference before completing its definition" stx)))
    (D:definition lhs env.d ^rhs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define ((auxiliary?/vocab vocab) a env stx.id)
  (and (identifier? stx.id) (eqv? (env-vocabulary-ref env stx.id vocab) a)))
(define expression-auxiliary? (auxiliary?/vocab vocab.expression-auxiliary))

(define (parse-expression* env stx*) (map (lambda (stx) (parse-expression env stx)) stx*))

(define-values (parse-error:kind parse-error? parse-error-syntax)
  (make-exception-kind-etc error:kind 'parse-error '#(syntax)))
(define (make-parse-error desc stx) (make-exception parse-error:kind (vector desc stx)))
(define (raise-parse-error . arg*) (raise (apply make-parse-error arg*)))

(define-values (unbound-identifier-parse-error:kind
                 unbound-identifier-parse-error?
                 unbound-identifier-parse-error-vocab
                 unbound-identifier-parse-error-env)
  (make-exception-kind-etc parse-error:kind 'unbound-identifier-parse-error '#(vocab env)))
(define (make-unbound-identifier-parse-error desc stx vocab env)
  (make-exception unbound-identifier-parse-error:kind (vector desc stx vocab env)))
(define (raise-unbound-identifier-parse-error . arg*)
  (raise (apply make-unbound-identifier-parse-error arg*)))

(define (parse-free-variable-reference env stx)
  (let ((desc "identifier is not bound as an expression"))
    (with-use-value
      '(replace unbound-variable-reference)
      (lambda ()
        (with-continue
          '(return unbound-variable-reference)
          (lambda () (raise-unbound-identifier-parse-error desc stx vocab.expression env)))
        ($mistake ($quote 'unbound-variable-reference)
                  ($quote (list desc stx vocab.expression env)))))))

(define (parse-expression env stx)
  ($source
    (let ((x (syntax-unwrap stx)))
      (cond
        ((identifier? stx)
         (let ((op (env-vocabulary-ref env stx vocab.expression)))
           (if (procedure? op) (op env stx) (parse-free-variable-reference env stx))))
        ((pair? x)
         (let* ((e.op (car x))
                (op   (and (identifier? e.op)
                           (env-vocabulary-ref env e.op vocab.expression-operator))))
           (if (procedure? op)
               (op env stx)
               ($call* (parse-expression env e.op)
                       (parse-expression* env (syntax->list (cdr x)))))))
        ((literal? x) ($quote x))
        (else (raise-parse-error "not an expression" stx))))
    stx))

(define ((expression-operator-parser parser argc.min argc.max) env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-parse-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-parse-error "too many operator arguments" stx))
    (apply parser env (cdr stx*))))

(define ((parse/constant-expression E) env _) E)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-definition env.d env stx)
  (define (default) (parse-definition-expression env.d env stx))
  (let ((x (syntax-unwrap stx)))
    (cond ((identifier? stx)
           (let ((op (env-vocabulary-ref env stx vocab.definition)))
             (if (procedure? op) (op env.d env stx) (default))))
          ((pair? x)
           (let* ((stx.op (car x))
                  (op     (and (identifier? stx.op)
                               (env-vocabulary-ref env stx.op vocab.definition-operator))))
             (if (procedure? op) (op env.d env stx) (default))))
          (else (default)))))

(define (parse-definition-expression env.d env stx)
  ($d:expression (lambda () (parse-expression env stx))))

(define ((definition-operator-parser parser argc.min argc.max) env.d env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-parse-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-parse-error "too many operator arguments" stx))
    (apply parser env.d env (cdr stx*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing assignments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-set! env stx.lhs stx.rhs)
  (define (fail) (raise-parse-error "not assignable" stx.lhs))
  (define (^rhs) (parse-expression env stx.rhs))
  (let ((x.lhs (syntax-unwrap stx.lhs)))
    (cond
      ((identifier? stx.lhs) (let ((op (env-vocabulary-ref env stx.lhs vocab.set!)))
                               (unless (procedure? op) (fail))
                               (op env stx.lhs (^rhs))))
      ((pair? x.lhs)
       (let* ((stx.op (car x.lhs))
              (op     (and (identifier? stx.op)
                           (env-vocabulary-ref env stx.op vocab.set!-operator))))
         (unless (procedure? op) (fail))
         (op env stx.lhs (^rhs))))
      (else (fail)))))
