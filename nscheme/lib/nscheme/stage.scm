(provide stage env:initial env:primitive binding:syntax/validation language)

(require length=? length>=? param?! bpair*?! param-map param-names
         ctx:var ctx:set! ctx:op ctx:def
         env:empty env-ref env-get-prop env-extend* env-update*
         defstate:empty defstate-env defstate-names defstate-actions
         defstate-env-set defstate-names-add defstate-actions-add
         ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:prim ast:context
         primitive-op-descriptions primitive-op-type-signature)

(define (binding:var n r) (cons n (list (cons ctx:var r) (cons ctx:set! r))))
(define (binding:syntax ctx n proc) (cons n (list (cons ctx proc))))
(define (env-extend*/var env n*)
  (param?! n*)
  (env-extend* env (map (lambda (n) (binding:var n (make-mvector 1 n))) n*)))
(define (param/renamings env param)
  (param-map (lambda (n) (env-get-prop env n ctx:var #f)) param))

;; High-level AST construction
(define ast:null        (ast:quote '()))
(define ast:true        (ast:quote #t))
(define ast:false       (ast:quote #f))
(define (ast:cons a d)  (ast:prim 'cons (list a d)))
(define (ast:list . xs) (foldr ast:cons ast:null xs))
(define (ast:vector . xs)
  (define vargs (list (ast:quote (length xs)) ast:true))
  (define $mv (ast:var 'mv))
  (define (! i x) (ast:prim 'mvector-set! (list $mv (ast:quote i) x)))
  (ast:let '(mv) (list (ast:prim 'make-mvector vargs))
           (ast:begin (append (map ! (range (length xs)) xs)
                              (list (ast:prim 'mvector->vector (list $mv)))))))
(define (ast:apply* $proc $a*) (ast:apply $proc (apply ast:list $a*)))
(define (ast:let p* v* body)   (ast:apply* (ast:lambda p* body) v*))
(define (ast:begin a*)
  (define ra* (reverse (cons ast:true a*)))
  (foldl (lambda (a rest) (ast:let '(#f) (list a) rest)) (car ra*) (cdr ra*)))
(define (ast:shift proc) (ast:context 'shift (list proc)))
(define (ast:reset body) (ast:context 'reset (list (ast:lambda '() body))))

;; Staging
(define (stage env form)
  (cond ((pair? form)
         (let ((p (car form)) (a* (cdr form)))
           (let ((op (and (string? p) (env-get-prop env p ctx:op #f))))
             (if op (apply op env a*)
               (ast:apply* (stage env p) (stage* env a*))))))
        ((string? form) (ast:var (or (env-get-prop env form ctx:var #f)
                                     (error '"unbound variable:" form))))
        ((or (boolean? form) (number? form)) (ast:quote form))
        ((procedure? form)                   (form env))
        (#t                                  (error '"invalid syntax:" form))))
(define (stage* env form*) (map (lambda (f) (stage env f)) form*))

(define (@apply env proc arg) (ast:apply (stage env proc) (stage env arg)))
(define (@quote env datum)    (ast:quote datum))
(define (@reset env . body)   (ast:reset (@body* env body)))
(define (@shift env k . body) (ast:shift (apply @lambda env (list k) body)))
(define (@set! env param arg)
  (define (setter n) (or (env-get-prop env n ctx:set! #f)
                         (error '"cannot set! variable:" n)))
  (ast:set! (param-map setter param) (stage env arg)))
(define (@if env c t f) (ast:if (stage env c) (stage env t) (stage env f)))
(define (@lambda env param . body)
  (define benv (env-extend*/var env (param-names param)))
  (ast:lambda (param/renamings benv param) (@body* benv body)))
(define (@let/ env b* . body)
  (bpair*?! b*)
  (ast:apply* (apply @lambda env (map car b*) body)
              (stage* env (map cadr b*))))
(define (@let/name env n b* . body)
  (bpair*?! b*)
  (define (p env) (apply @lambda env (map car b*) body))
  (ast:apply* (@letrec env (list (list n p)) n) (stage* env (map cadr b*))))
(define (@let env . tail)
  (cond ((and (length>=? 2 tail) (string? (car tail)) (bpair*?! (cadr tail)))
         (apply @let/name env tail))
        (#t (bpair*?! (car tail)) (apply @let/ env tail))))
(define (@letrec env b* . body)
  (bpair*?! b*)
  (define (k env) (ast:begin (append (map (lambda (b) (apply @set! env b)) b*)
                                     (list (apply @let env '() body)))))
  (@let env (map (lambda (b) (list (car b) #t)) b*) k))
(define (@let* env b* . body)
  (bpair*?! b*)
  (let loop ((b* b*) (env env))
    (if (null? b*) (@body* env body)
      (@let/ env (list (car b*)) (lambda (env) (loop (cdr b*) env))))))
(define (@and env . args)
  (define ra* (reverse (cons #t args)))
  (foldl (lambda (a rest) (@if env a (lambda (_) rest) #f))
         (stage env (car ra*)) (cdr ra*)))
(define (@or env . args)
  (foldr (lambda (arg rest)
           (@let env (list (list 'temp arg))
                 (lambda (env) (@if env 'temp 'temp (lambda (_) rest)))))
         ast:false args))
(define (@cond env . clauses)
  (foldr (lambda (c rest)
           (unless (length>=? 1 c) (error '"invalid cond clause:" c))
           (if (null? (cdr c)) (@or env (car c) (lambda (_) rest))
             (@if env (car c) (lambda (env) (@body* env (cdr c)))
                  (lambda (_) rest)))) ast:true clauses))
(define (@begin env . body)    (ast:begin (stage* env body)))
(define (@when env c . body)   (@if env c (lambda (env) (@body* env body)) #t))
(define (@unless env c . body) (@if env c #t (lambda (env) (@body* env body))))

(define (defstate-actions-add-expr st form)
  (defstate-actions-add st (lambda (env) (stage env form))))
(define (defstate-run st)
  (let ((actions (reverse (defstate-actions st)))
        (env (defstate-env st)) (names (defstate-names st)))
    (ast:let (param/renamings env names) (map (lambda (_) ast:true) names)
             (ast:begin (map (lambda (act) (act env)) actions)))))
(define (@begin/define st . forms)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($def (and n (env-get-prop (defstate-env st) n ctx:def #f))))
             (if $def (apply $def st (cdr form))
               (defstate-actions-add-expr st form)))) st forms))
(define (@def st param arg)
  (define names (param-names param))
  (define env (env-extend*/var (defstate-env st) names))
  (defstate-actions-add (defstate-env-set (defstate-names-add st names) env)
                        (lambda (env) (@set! env param arg))))
(define (@define st param . body)
  (if (pair? param)
    (@define st (car param)
             (lambda (env) (apply @lambda env (cdr param) body)))
    (apply @def st param body)))
(define (@body* env body*)
  (defstate-run (apply @begin/define (defstate:empty env) body*)))

;; Initial environment definition
(define (binding:syntax/validation ctx name proc arity exact?)
  (binding:syntax
    ctx name (lambda (env . tail)
               (unless ((if exact? length=? length>=?) arity tail)
                 (error '"invalid syntax arity:" arity (cons name tail)))
               (apply proc env tail))))
(define initial-syntax-bindings
  (map (lambda (desc) (apply binding:syntax/validation (cons ctx:op desc)))
       (list (list 'apply  @apply  2 #t)
             (list 'quote  @quote  1 #t)
             (list 'if     @if     3 #t)
             (list 'set!   @set!   2 #t)
             (list 'reset  @reset  0 #f)
             (list 'shift  @shift  1 #f)
             (list 'lambda @lambda 1 #f)
             (list 'letrec @letrec 1 #f)
             (list 'let    @let    1 #f)
             (list 'let*   @let*   1 #f)
             (list 'begin  @begin  0 #f)
             (list 'cond   @cond   0 #f)
             (list 'and    @and    0 #f)
             (list 'or     @or     0 #f)
             (list 'when   @when   1 #f)
             (list 'unless @unless 1 #f))))
(define env:initial
  (env-update* (env-extend* env:empty initial-syntax-bindings)
               (list (binding:syntax ctx:def 'begin  @begin/define)
                     (binding:syntax ctx:def 'define @define)
                     (binding:syntax ctx:def 'def    @def))))

;; Primitive environment definition
(define primitive-syntax-bindings
  (map (lambda (po-desc)
         (binding:syntax
           ctx:op (car po-desc)
           (lambda (env . tail)
             (define type-sig (primitive-op-type-signature po-desc))
             (unless (length=? (length (car type-sig)) tail)
               (error '"invalid primitive op:" po-desc tail))
             (ast:prim (car po-desc) (stage* env tail)))))
       primitive-op-descriptions))
(define env:primitive (env-extend* env:initial primitive-syntax-bindings))

;; Language construction
(define (language env public-names? binding-groups renamings->bindings:syntax)
  (define all-names
    (foldl append '() (map (lambda (grp) (map car (cdr grp))) binding-groups)))
  (define public-names (or public-names? all-names))
  (define private-names
    (if public-names?
      (filter (lambda (n) (not (member n public-names))) all-names) '()))
  (define public-renames #t)
  (define all-renames #t)
  (define (body env)
    (define private-renames (param/renamings env private-names))
    (set! public-renames (param/renamings env public-names))
    (set! all-renames (append private-renames public-renames))
    (apply ast:list (map ast:var all-renames)))
  (define (bind-group group body)
    (define @bind (cond ((equal? (car group) 'let)    @let/)
                        ((equal? (car group) 'letrec) @letrec)
                        (#t (error '"invalid binding group:" group))))
    (lambda (env) (@bind env (cdr group) body)))
  (define ast:values (stage env (foldr bind-group body binding-groups)))
  (define bindings:syntax
    (renamings->bindings:syntax (map cons all-names all-renames)))
  (define (stager env body)
    (define env:language
      (env-update*
        (env-extend* env (map binding:var public-names public-renames))
        bindings:syntax))
    (ast:lambda all-renames (stage env:language body)))
  (vector stager ast:values))
