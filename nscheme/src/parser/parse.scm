;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-values (parse-error:kind parse-error? parse-error-syntax)
  (make-exception-kind-etc error:kind 'parse-error '#(syntax)))
(define (make-parse-error desc stx) (make-exception parse-error:kind (vector desc stx)))
(define (raise-parse-error . arg*) (raise (apply make-parse-error arg*)))

(define (syntax->improper-list s)
  (let ((x (syntax-unwrap s)))
    (cond ((pair? x) (cons (car x) (syntax->improper-list (cdr x))))
          ((null? x) '())
          (else      s))))

(define (syntax->list s)
  (let ((x*~ (syntax->improper-list s)))
    (unless (list? x*~) (raise-parse-error "not a syntax list" s))
    x*~))

(define ((syntax->operand*/minmax argc.min argc.max) stx)
  (when (identifier? stx) (raise-parse-error "operator identifier not used in an operation" stx))
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-parse-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-parse-error "too many operator arguments" stx))
    (cdr stx*)))

(define ((operator-parser parser argc.min argc.max) env stx)
  (apply parser env ((syntax->operand*/minmax argc.min argc.max) stx)))

(define (parse-identifier id) (unless (identifier? id) (raise-parse-error "not an identifier" id)))

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

(define ((vocabulary-parser vocab ^default) env stx)
  (define (syntax-operator stx)
    (let ((op (and (identifier? stx) (env-vocabulary-ref env stx vocab))))
      (when (and op (not (procedure? op)))
        (raise-parse-error (list "misplaced keyword" vocab op) stx))
      op))
  ((or (syntax-operator stx)
       (let ((x (syntax-unwrap stx))) (and (pair? x) (syntax-operator (car x))))
       (^default))
   env stx))

;;;;;;;;;;;;;;;;
;;; Bindings ;;;
;;;;;;;;;;;;;;;;
(define (make-binding id) '())
(define (binding-vocabulary-ref v=>x vocab) (let ((vx (assv vocab v=>x))) (and vx (cdr vx))))
(define (binding-vocabulary-remove v=>x id . vocab*) (binding-vocabulary-remove* v=>x id vocab*))
(define (binding-vocabulary-set    v=>x id . vx*)    (binding-vocabulary-set*    v=>x id vx*))
(define (binding-vocabulary-add    v=>x id . vx*)    (binding-vocabulary-add*    v=>x id vx*))
(define (binding-vocabulary-update v=>x id . vu*)    (binding-vocabulary-update* v=>x id vu*))
(define (binding-vocabulary-remove* v=>x id vocab*)
  (unless v=>x (mistake 'binding-vocabulary-remove* "unbound identifier" id))
  (filter (lambda (vx) (not (memv (car vx) vocab*))) v=>x))
(define (binding-vocabulary-set* v=>x id vx*)
  (unless v=>x (mistake 'binding-vocabulary-set* "unbound identifier" id))
  (let ((v&x* (plist->alist vx*)))
    (append v&x* (binding-vocabulary-remove* v=>x id (map car v&x*)))))
(define (binding-vocabulary-add* v=>x id vx*)
  (unless v=>x (mistake 'binding-vocabulary-add* "unbound identifier" id))
  (for-each
    (lambda (vocab)
      (when (binding-vocabulary-ref v=>x vocab)
        (mistake 'binding-vocabulary-add* "identifier already bound with vocabulary" id vocab)))
    (plist-key* vx*))
  (binding-vocabulary-set* v=>x id vx*))
(define (binding-vocabulary-update* v=>x id vu*)
  (unless v=>x (mistake 'binding-vocabulary-update* "unbound identifier" id))
  (let* ((vocab* (plist-key* vu*))
         (x*     (map (lambda (vocab)
                        (or (binding-vocabulary-ref v=>x vocab)
                            (mistake 'binding-vocabulary-update*
                                     "identifier not bound with vocabulary" id vocab)))
                      vocab*))
         (x*     (map (lambda (update x) (update x)) (plist-value* vu*) x*)))
    (binding-vocabulary-set* v=>x id (alist->plist (map cons vocab* x*)))))

;;;;;;;;;;;;;;;;;;;;
;;; Vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;
(define vocab.definition 'definition)
(define vocab.expression 'expression)
(define vocab.set!       'set!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (env-vocabulary-ref env id vocab)
  (let ((vocab=>x (env-ref env id))) (and vocab=>x (binding-vocabulary-ref vocab=>x vocab))))
(define (env-vocabulary-bind!   env id . vx*)    (env-vocabulary-bind!*   env id vx*))
(define (env-vocabulary-set!    env id . vx*)    (env-vocabulary-set!*    env id vx*))
(define (env-vocabulary-add!    env id . vx*)    (env-vocabulary-add!*    env id vx*))
(define (env-vocabulary-remove! env id . vocab*) (env-vocabulary-remove!* env id vocab*))
(define (env-vocabulary-update! env id . vu*)    (env-vocabulary-update!* env id vu*))
(define (env-vocabulary-bind!* env id vx*)
  (env-bind! env id (binding-vocabulary-set* (make-binding id) id vx*)))
(define (env-vocabulary-set!* env id vx*)
  (env-rebind! env id (binding-vocabulary-set* (or (env-ref env id) (make-binding id)) id vx*)))
(define (env-vocabulary-add!* env id vx*)
  (env-rebind! env id (binding-vocabulary-add* (or (env-ref env id) (make-binding id)) id vx*)))
(define (env-vocabulary-remove!* env id vocab*)
  (env-rebind! env id (binding-vocabulary-remove* (or (env-ref env id) (make-binding id)) id vocab*)))
(define (env-vocabulary-update!* env id vu*)
  (env-rebind! env id (binding-vocabulary-update* (env-ref env id) id vu*)))

(define (env-extend env param* E*)
  (parse-param* param*)
  (env-conjoin (let ((env.scope (make-env)))
                 (for-each (lambda (id E) (env-vocabulary-bind! env.scope id vocab.expression
                                                                (parse/constant-expression E)))
                           param* E*)
                 (env-read-only! env.scope)
                 env.scope)
               env))

(define (env-bind-boxed! env id ^E.box)
  (env-vocabulary-bind!
    env id
    vocab.expression (expression-identifier-parser (lambda (env _) ($unbox (^E.box))))
    vocab.set!       (set!-identifier-parser
                       (lambda (env stx.lhs)
                         ($lambda (list 'rhs) (lambda (E.rhs)
                                                ($set-box! ($source (^E.box) stx.lhs) E.rhs)))))))

(define (env-add-alist! env a)
  (alist-for-each a (lambda (id E) (env-vocabulary-bind!
                                     env id vocab.expression (parse/constant-expression E)))))
(define (alist->env a) (let ((env (make-env))) (env-add-alist! env a) (env-read-only! env) env))
(define (env-add-value-alist! env a) (env-add-alist! env (alist-map-value a $quote)))
(define (value-alist->env a) (alist->env (alist-map-value a $quote)))
(define (addr-alist->env  a) (alist->env (alist-map-value a $ref)))

(define (((keyword/env/vocabulary vocab) env) stx.id)
  (and (identifier? stx.id) (env-vocabulary-ref env stx.id vocab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax transformation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ((syntax-transcribe/parse parse) stx op env.op env.use)
  (let-values (((stx env) (syntax-transcribe stx op env.op env.use))) (parse env stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expression construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ($annotate     E note)        (E-annotate E note))
(define ($quote        value)         (E:quote        value))
(define ($ref          addr)          (E:ref          addr))
(define ($if           c t f)         (E:if           c t f))
(define ($apply/values rator vrand)   (E:apply/values rator vrand))
(define ($call*        rator rand*)   (E:call         rator rand*))
(define ($call         rator . rand*) ($call* rator rand*))

(splicing-local
  ((define (param->address stx)
     (let ((x (syntax-unwrap stx)))
       (unless (or (symbol? x) (not x)) (mistake "not a formal parameter" stx))
       (make-address x stx))))
  (define ($case-lambda param*~* ^body*)
    (let* ((addr*~* (map (lambda (param) (improper-list-map param->address param)) param*~*))
           (body*   (map (lambda (addr*~ ^body)
                           (apply ^body (map $ref (improper-list->list addr*~))))
                         addr*~* ^body*)))
      (E:case-lambda addr*~* body*)))
  (define ($letrec param* ^rhs*&body)
    (let ((addr* (map param->address param*)))
      (let-values (((rhs* body) (apply ^rhs*&body (map $ref addr*))))
        (E:letrec addr* rhs* body)))))

(define ($lambda param*~     ^body) ($case-lambda (list param*~) (list ^body)))
(define ($let    param* rhs* ^body) ($call* ($lambda param* ^body) rhs*))
(define ($begin  e . e*)            (let loop ((e e) (e* e*))
                                      (if (null? e*) e (E:seq e (loop (car e*) (cdr e*))))))

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
                (if (null? a*) a ($if a (loop (car a*) (cdr a*)) ($quote #f)))))))
(define $or
  (case-lambda
    (()       ($quote #f))
    ((a . a*) (let loop ((a a) (a* a*))
                (if (null? a*) a ($let1 't a (lambda ($t)
                                               ($if $t $t (loop (car a*) (cdr a*))))))))))
(define ($not                 x) ($if x ($quote #f) ($quote #t)))
(define ($when           c body) ($if c body ($values)))
(define ($unless         c body) ($when ($not c) body))
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
(define ($list->vector      x)   ($pcall apply ($quote vector) x))
(define ($vector->list      x)   ($pcall vector->list  x))
(define ($vector-ref        v i) ($pcall vector-ref    v i))
(define ($vector-length     v)   ($pcall vector-length v))
(define ($vector           . x*) (apply $pcall vector x*))
(define ($values           . x*) (apply $pcall values x*))
(define ($quote-values     . x*) (apply $values (map $quote x*)))
(define ($mistake     . detail*) (apply $pcall panic ($quote 'mistake) detail*))
(define ($list             . x*) (let loop ((x* x*))
                                   (cond ((null? x*) ($quote '()))
                                         (else       ($cons (car x*) (loop (cdr x*)))))))
(define ($syntax-unwrap     x)   ($pcall syntax-unwrap x))
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
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (literal? x) (or (boolean? x) (number? x) (string? x) (bytevector? x)))

(define current-parse-free-variable
  (make-parameter
    (lambda (env id)
      (raise-parse-error
        (if (env-ref env id) (list "misplaced keyword" vocab.expression) "unbound identifier")
        id))))

(define (parse-expression* env stx*) (map (lambda (stx) (parse-expression env stx)) stx*))
(define (parse-expression env stx)
  (define ((^default) env stx)
    (let* ((x (syntax-unwrap stx)))
      (cond ((pair?    x) ($call* (parse-expression env (car x)) (parse-expression* env (syntax->list (cdr x)))))
            ((literal? x) ($quote x))
            ((symbol?  x) ((current-parse-free-variable) env stx))
            (else         (raise-parse-error (list vocab.expression "invalid syntax") stx)))))
  ($source ((vocabulary-parser vocab.expression ^default) env stx) stx))

(define (parse-call env stx.op stx.rand*)
  ($call* (parse-expression env stx.op) (parse-expression* env (syntax->list stx.rand*))))

(define ((expression-identifier-parser parse) env stx)
  (if (identifier? stx)
      (parse env stx)
      (let ((x (syntax-unwrap stx))) (parse-call env (car x) (cdr x)))))

(define (parse/constant-expression E) (expression-identifier-parser (lambda (env stx) E)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing assignments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-set! env stx.lhs stx.rhs)
  (define (fail) (raise-parse-error "not assignable" stx.lhs))
  ($call ((vocabulary-parser vocab.set! fail) env stx.lhs) (parse-expression env stx.rhs)))

(define ((set!-identifier-parser parse) env stx.lhs)
  (unless (identifier? stx.lhs) (raise-parse-error (list vocab.set! "not an identifier") stx.lhs))
  (parse env stx.lhs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-definition env stx)
  ((vocabulary-parser vocab.definition (lambda () parse-definition-expression)) env stx))

(define (parse-definition-expression env stx) ($d:expression (let ((env (env-read-only env)))
                                                               (lambda () (parse-expression env stx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition contexts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define (D:begin          D*)        (vector 'D:begin          D*))
   (define (D:definition     id b ^rhs) (vector 'D:definition     id b ^rhs))
   (define (D:expression     ^E)        (vector 'D:expression     ^E))
   (define (D:end/expression D stx)     (vector 'D:end/expression D stx))
   (define (D-tag                   D) (vector-ref D 0))
   (define (D:begin-D*              D) (vector-ref D 1))
   (define (D:definition-id         D) (vector-ref D 1))
   (define (D:definition-binding    D) (vector-ref D 2))
   (define (D:definition-^rhs       D) (vector-ref D 3))
   (define (D:expression-^E         D) (vector-ref D 1))
   (define (D:end/expression-D      D) (vector-ref D 1))
   (define (D:end/expression-syntax D) (vector-ref D 2))
   (define (D-tagged? D tag) (eqv? (D-tag D) tag))
   (define (definition id b ^E)  (vector id b ^E))
   (define (definition-id   entry) (vector-ref entry 0))
   (define (definition-E!   entry) (vector-ref entry 1))
   (define (definition-^rhs entry) (vector-ref entry 2))
   (define (D-compile D publish-values?)
     (mlet ((rdef* '()) (^E.current #f))
       (let loop! ((D D))
         (cond ((D-tagged? D 'D:begin) (for-each loop! (D:begin-D* D)))
               ((D-tagged? D 'D:definition)
                (let ((^E   ^E.current)
                      (^E.D (D:definition-^rhs D)))
                  (set! rdef* (cons (definition (D:definition-id D) (D:definition-binding D)
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
                    (E!*   (map definition-E!   def*))
                    (^rhs* (map definition-^rhs def*))
                    (E*->E (if publish-values?
                               (lambda (E*)
                                 (define (publish-rhs*! rhs*)
                                   (for-each (lambda (E! rhs) (E! ($quote rhs))) E!* rhs*))
                                 ($let '(result* rhs*)
                                       (list ($apply/values
                                               ($lambda 'result* (lambda ($result*) $result*))
                                               (^E))
                                             (apply $list E*))
                                       (lambda ($result* $rhs*)
                                         ($begin ($pcall publish-rhs*! $rhs*)
                                                 ($pcall apply ($quote values) $result*)))))
                               (lambda (E*) (^E)))))
               ($letrec id* (lambda E*
                              (for-each (lambda (E! E) (E! E)) E!* E*)
                              (values (map (lambda (^rhs) (^rhs)) ^rhs*) (E*->E E*))))))))))
  (define (D->E         D) (D-compile D #f))
  (define (D->E/publish D) (D-compile D #t))
  (define ($d:begin . D*)           (D:begin D*))
  (define ($d:end/expression D stx) (D:end/expression D stx))
  (define ($d:expression ^E)        (D:expression ^E))
  (define ($d:define/vocabulary . vp*) ($d:define/vocabulary* vp*))
  (define (($d:define/vocabulary* vp*) env lhs ^rhs)
    (parse-identifier lhs)
    (mlet ((E #f))
      (define ((make-parser E->parse) env stx)
        ((E->parse (or E (raise-parse-error "parsed identifier before completing its definition" stx)))
         env stx))
      (env-vocabulary-bind!* env lhs (plist-map-value vp* make-parser))
      (D:definition lhs (lambda (E.new) (set! E E.new)) ^rhs)))
  (define $d:define ($d:define/vocabulary vocab.expression parse/constant-expression)))
