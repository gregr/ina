#lang racket/base
(module stage racket/base
  (provide alist-ref* ast-eval stage @lambda env:initial
           base:names base:values base:program base:eval stage:test!)
  (require
    (rename-in "interop.rkt"
               (nscm:equal? equal?) (nscm:member member) (nscm:assoc assoc)
               (nscm:quote quote) (nscm:quasiquote quasiquote))
    racket/list racket/string racket/vector)

  (define (alist-get rs key default)
    (define rib (assoc key rs))
    (if rib (cdr rib) default))
  (define (alist-ref alist k)
    (cdr (or (assoc k alist)
             (error "alist-ref of non-existent key:" k alist))))
  (define (alist-ref* alist k*) (map (lambda (k) (alist-ref alist k)) k*))
  (define (alist-remove* rs keys)
    (filter (lambda (rib) (not (member (car rib) keys))) rs))
  (define (vector-set v i x)
    (let ((result (vector-copy v))) (vector-set! result i x) result))

  ;; Pattern matching
  (define (length=? n xs)  (and (list? xs) (= (length xs) n)))
  (define (length>=? n xs) (and (list? xs) (>= (length xs) n)))
  (define (name->sym n) (string->symbol (if (mvector? n) (mvector-ref n 0) n)))
  (define (param?! param) (unless (andmap string? (param-names param))
                            (error '"invalid parameters:" param)))
  (define (bpair*?! b*)
    (define (? b) (and (length=? 2 b) (param?! (car b))))
    (unless (and (list? b*) (andmap ? b*))
      (error '"invalid binding list:" b*)))

  ;; Formal parameters
  (define (ncons name names)
    (when (member name names) (error '"duplicate name:" name names))
    (cons name names))
  (define (param-map f p)
    (cond ((pair? p)   (cons (param-map f (car p)) (param-map f (cdr p))))
          ((vector? p) (list->vector (param-map f (vector->list p))))
          ((null? p)   '())
          ((not p)     #f)
          (#t          (f p))))
  (define (param-names param)
    (let loop ((p param) (ns '()))
      (cond ((pair? p)   (loop (cdr p) (loop (car p) ns)))
            ((vector? p) (loop (vector->list p) ns))
            ((null? p)   ns)
            ((not p)     ns)
            (#t          (ncons p ns)))))
  (define (param-bind param arg)
    (let loop ((p param) (a arg))
      (cond ((and (pair? p) (pair? a)) (append (loop (car p) (car a))
                                               (loop (cdr p) (cdr a))))
            ((and (vector? p) (vector? a))
             (loop (vector->list p) (vector->list a)))
            ((and (null? p) (null? a)) '())
            ((not p)                   '())
            ((not (or (pair? p) (vector? p) (null? p))) (list (cons p a)))
            (#t (error '"parameter/argument mismatch:"
                       (param-map name->sym param) arg
                       (param-map name->sym p) a)))))

  (define ctx:var  '"ref")
  (define ctx:set! '"set!")
  (define ctx:op   '"syntax?")
  (define ctx:def  '"define")
  (define env:empty                      '())
  (define (env-ref env n)                (alist-get env n '()))
  (define (env-get-prop env n k default) (alist-get (env-ref env n) k default))
  (define (env-remove* env n*)           (alist-remove* env n*))
  (define (env-add* env b*)              (append b* env))
  (define (env-extend* env b*) (env-add* (env-remove* env (map car b*)) b*))

  (define (defstate:empty env)  (vector env '() '()))
  (define (defstate-env st)     (vector-ref st 0))
  (define (defstate-names st)   (vector-ref st 1))
  (define (defstate-actions st) (vector-ref st 2))
  (define (defstate-env-set st env)
    (vector env (defstate-names st) (defstate-actions st)))
  (define (defstate-names-add st names)
    (define new (foldl ncons (defstate-names st) names))
    (vector (defstate-env st) new (defstate-actions st)))
  (define (defstate-actions-add st act)
    (define new (cons act (defstate-actions st)))
    (vector (defstate-env st) (defstate-names st) new))

  ;; Generated code with procedural representation
  (define ast-env:empty (hasheqv))  ;; eqv assumes mvector keys
  (define (ast-env-extend* env b*)
    (foldl (lambda (b e)
             (define cell    (make-mvector 1 (cdr b)))
             (define (get)   (mvector-ref  cell 0))
             (define (set v) (mvector-set! cell 0 v))
             (hash-set e (car b) (cons get set))) env b*))
  (define (env-ref-capabilities env addr)
    (or (hash-ref env addr #f) (error '"unbound address:" addr)))
  (define (ast-env-ref env addr)    ((car (env-ref-capabilities env addr))))
  (define (ast-env-set! env addr v) ((cdr (env-ref-capabilities env addr)) v))
  (define (ast-eval ast) (ast ast-env:empty))

  (define (ast:quote datum)       (lambda (env) datum))
  (define (ast:var address)       (lambda (env) (ast-env-ref env address)))
  (define (ast:if c t f)          (lambda (env) (if (c env) (t env) (f env))))
  (define (ast:apply proc arg)    (lambda (env) ($apply (proc env) (arg env))))
  (define (ast:lambda param body)
    (lambda (env) (lambda (arg)
                    (define b* (param-bind param arg))
                    (body (ast-env-extend* env b*)))))
  (define (ast:set! param arg)
    (lambda (env)
      (define (! b) (ast-env-set! env (car b) (cdr b)))
      (for-each ! (param-bind param (arg env)))))

  (define (env-extend*/var env n*)
    (param?! n*)
    (define (bind n)
      (define rn (make-mvector 1 n))
      (cons n (list (cons ctx:var rn) (cons ctx:set! rn))))
    (env-extend* env (map bind n*)))
  (define (env-extend*/syntax env ctx b*)
    (define (bind b) (let ((n (car b)))
                       (cons n (cons (cons ctx (cdr b)) (env-ref env n)))))
    (env-extend* env (map bind b*)))
  (define (param/renamings env param)
    (param-map (lambda (n) (env-get-prop env n ctx:var #f)) param))

  ;; High-level AST construction
  (define ast:null        (ast:quote '()))
  (define ast:true        (ast:quote #t))
  (define ast:false       (ast:quote #f))
  (define (ast:cons a d)  (lambda (env) (cons (a env) (d env))))
  (define (ast:list . xs) (foldr ast:cons ast:null xs))
  (define (ast:apply* $proc $a*) (ast:apply $proc (apply ast:list $a*)))
  (define (ast:let p* v* body)   (ast:apply* (ast:lambda p* body) v*))
  (define (ast:begin a*)
    (define ra* (reverse (cons ast:true a*)))
    (foldl (lambda (a rst) (ast:let '(#f) (list a) rst)) (car ra*) (cdr ra*)))
  (define (ast:shift proc) (lambda (env) (shift (proc env))))
  (define (ast:reset body)
    (define proc (ast:lambda '() body))
    (lambda (env) (reset (proc env))))

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
          (#t (error '"invalid syntax:" form))))
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
    (define (k env)
      (ast:begin (append (map (lambda (b) (apply @set! env b)) b*)
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
             (@let env (list (list '"temp" arg))
                   (lambda (env) (@if env '"temp" '"temp" (lambda (_) rest)))))
           ast:false args))
  (define (@cond env . clauses)
    (foldr (lambda (c rest)
             (unless (length>=? 1 c) (error '"invalid cond clause:" c))
             (ast:if (stage env (car c)) (@body* env (cdr c)) rest))
           ast:true clauses))
  (define (@begin e . body)  (ast:begin (stage* e body)))
  (define (@when e c . body)   (@if e c (lambda (e) (@body* e body)) #t))
  (define (@unless e c . body) (@if e c #t (lambda (e) (@body* e body))))

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
                    ($def (and n (env-get-prop
                                   (defstate-env st) n ctx:def #f))))
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

  ;; Initial language definition
  (define (stager:initial-syntax name proc arity exact?)
    (cons name (lambda (env . tail)
                 (unless ((if exact? length=? length>=?) arity tail)
                   (error '"invalid syntax arity:" arity (cons name tail)))
                 (apply proc env tail))))
  (define initial-syntax-bindings
    (map (lambda (desc) (apply stager:initial-syntax desc))
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
    (env-extend*/syntax
      (env-extend*/syntax env:empty ctx:op initial-syntax-bindings)
      ctx:def (list (cons 'begin  @begin/define)
                    (cons 'define @define)
                    (cons 'def    @def))))

  ;; Base library and program definition
  (define base:bindings
    (map (lambda (b) (cons (car b) (lift (cdr b))))
         (list (cons 'procedure?      procedure?)
               (cons 'mvector?        mvector?)
               (cons 'vector?         vector?)
               (cons 'pair?           pair?)
               (cons 'null?           null?)
               (cons 'boolean?        boolean?)
               (cons 'string?         string?)
               (cons 'number?         number?)
               (cons 'integer?        integer?)
               (cons 'fixnum?         fixnum?)
               (cons 'flonum?         flonum?)
               (cons 'boolean=?       boolean=?)
               (cons 'number=?        number=?)
               (cons 'string=?        string=?)
               (cons 'mvector=?       mvector=?)
               (cons 'procedure=?     procedure=?)
               (cons 'string->vector  string->vector)
               (cons 'vector->string  vector->string)
               (cons 'cons            cons)
               (cons 'car             car)
               (cons 'cdr             cdr)
               (cons 'vector-ref      vector-ref)
               (cons 'vector-length   vector-length)
               (cons 'make-mvector    make-mvector)
               (cons 'mvector->vector mvector->vector)
               (cons 'mvector-set!    mvector-set!)
               (cons 'mvector-ref     mvector-ref)
               (cons 'mvector-length  mvector-length)
               (cons 'string<?        string<?)
               (cons 'string>?        string>?)
               (cons '=               =)
               (cons '<=              <=)
               (cons '<               <)
               (cons '>=              >=)
               (cons '>               >)
               (cons '+               +)
               (cons '*               *)
               (cons '-               -)
               (cons '/               /)
               (cons 'truncate        truncate)

               (cons 'arithmetic-shift arithmetic-shift)
               (cons 'bitwise-and      bitwise-and)
               (cons 'bitwise-ior      bitwise-ior)
               (cons 'bitwise-xor      bitwise-xor)
               (cons 'bitwise-not      bitwise-not)
               (cons 'integer-length   integer-length)

               (cons 'apply         $apply)
               (cons 'error         (lambda args (error "error:" args)))
               (cons 'not           not)
               (cons 'caar          caar)
               (cons 'cadr          cadr)
               (cons 'cdar          cdar)
               (cons 'cddr          cddr)
               (cons 'caaar         caaar)
               (cons 'caadr         caadr)
               (cons 'cadar         cadar)
               (cons 'caddr         caddr)
               (cons 'cdaar         cdaar)
               (cons 'cdadr         cdadr)
               (cons 'cddar         cddar)
               (cons 'cdddr         cdddr)
               (cons 'list-tail     list-tail)
               (cons 'list-ref      list-ref)
               (cons 'list->vector  list->vector)
               (cons 'vector->list  vector->list)
               (cons 'equal?        equal?)
               (cons 'vector        vector)
               (cons 'vector-set    vector-set)
               (cons 'list?         list?)
               (cons 'list          list)
               (cons 'list*         list*)
               (cons 'remove        remove)
               (cons 'length        length)
               (cons 'append        append)
               (cons 'reverse       reverse)
               (cons 'range         range)
               (cons 'take          take)
               (cons 'drop          drop)
               (cons 'member        member)
               (cons 'assoc         assoc)
               (cons 'alist-get     alist-get)
               (cons 'alist-ref     alist-ref)
               (cons 'alist-ref*    alist-ref*)
               (cons 'alist-remove* alist-remove*)
               (cons 'string-append string-append)
               (cons 'string-split  string-split)
               (cons 'foldl         (lower-arg0 foldl))
               (cons 'foldr         (lower-arg0 foldr))
               (cons 'map           (lower-arg0 map))
               (cons 'for-each      (lower-arg0 for-each))
               (cons 'andmap        (lower-arg0 andmap))
               (cons 'ormap         (lower-arg0 ormap))
               (cons 'filter        (lower-arg0 filter))
               (cons 'filter-not    (lower-arg0 filter-not))
               (cons 'remf          (lower-arg0 remf))
               (cons 'memf          (lower-arg0 memf))
               )))

  (define base:names          (map car base:bindings))
  (define base:values         (map cdr base:bindings))
  (define (base:program form) (@lambda env:initial base:names form))
  (define (base:eval form) ($apply (ast-eval (base:program form)) base:values))

  (define (stage:test! test)
    (define ev base:eval)

    (test 'lambda-1  ;; Formal parameters are generalized to arbitrary trees.
      (ev '((lambda (() a (b)) (cons a b))
            '() 1 '(2)))
      '(1 . 2))
    (test 'lambda-2  ;; Formal parameters are generalized to arbitrary trees.
      (ev '((lambda (() a #(b c)) (cons a (cons b (cons c '()))))
            '() 1 '#(2 3)))
      '(1 2 3))
    (test 'lambda-3
      (ev '((lambda (#f x #f) x) 1 2 3))
      2)
    (test 'define-1
      (ev '((lambda (() a #(b c))
              (define x (lambda () (+ (+ c y) z)))
              (define y b)
              (define z a)
              (x))
            '() 1 '#(2 3)))
      6)
    (test 'define-2
      (ev '((lambda (() a #(b c))
              (begin (define x (lambda () (+ (+ c y) z)))
                     (define y b))
              (define z a)
              (x))
            '() 1 '#(2 3)))
      6)
    (test 'define-3
      (ev '((lambda (() a #(b c))
              (define ((f w) x y) (list w x y))
              ((f a) b c))
            '() 1 '#(2 3)))
      '(1 2 3))

    (test 'literals
      (map ev (list '(quote ()) '#t '4))
      '(() #t 4))

    (test 'pair-1
      (ev '(pair? '(x x)))
      #t)
    (test 'pair-2
      (ev '(pair? #t))
      #f)
    (test 'pair-3
      (ev '(pair? (lambda x x)))
      #f)

    (test 'procedure-1
      (ev '(procedure? '(x x)))
      #f)
    (test 'procedure-2
      (ev '(procedure? '#t))
      #f)
    (test 'procedure-3
      (ev '(procedure? (lambda x x)))
      #t)

    (test 'lambda-shadowing-1
      (ev '((lambda lambda lambda) 'ok))
      '(ok))

    (test 'lambda-app-1
      (ev '((lambda (x y) x) 5 6))
      5)
    (test 'lambda-app-2
      (ev '((lambda (x y) y) 5 6))
      6)
    (test 'lambda-app-3
      (ev '((lambda (x y) (cons y x)) 5 6))
      '(6 . 5))
    (test 'lambda-app-4
      (ev '((lambda (x) (cons (cdr x) (car x))) (cons #t #f)))
      '(#f . #t))

    (test 'let-1
      (ev '(let ((x 8)) x))
      8)
    (test 'let-2
      (ev '(let ((x 9)) (let ((x 20)) x)))
      20)
    (test 'let-3
      (ev '(let ((x 9)) (let ((y 20)) x)))
      9)
    (test 'let-4
      (ev '(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
      10)
    (test 'let-5
      (ev '(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
      (let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
    (test 'let-6
      (ev '(let ((op (lambda (x) (car x)))
                 (datum '(#t . #f)) (ta 'yes) (fa 'no))
             (if (op datum) ta fa)))
      'yes)
    (test 'let-7
      (ev '(let ((op (lambda (x) (cdr x)))
                 (datum '(#t . #f)) (ta 'yes) (fa 'no))
             (if (op datum) ta fa)))
      'no)
    (test 'let-8
      (ev '(let ((op (lambda (x) (cdr x)))
                 (datum '(#t . #f)) (ta 'yes) (fa 'no))
             (if (op datum) ta fa)))
      (let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
        (if (op datum) ta fa)))
    (test 'let-9
      (ev '(let loop ((xs '(a b c)) (acc '()))
             (if (null? xs) acc
               (loop (cdr xs) (cons (car xs) acc)))))
      '(c b a))

    (test 'internal-defs-1
      (ev '(let ((x 1) (y 7) (z 33))
             (define u 88)
             (define z y)
             6
             (begin (define a 5) (define w 4))
             z))
      7)
    (test 'internal-defs-2
      (ev '(let ((x 1) (y 7) (z 33))
             (define y 88)
             (define z y)
             6
             (begin (define a 5) (define w 4))
             z))
      88)
    (test 'internal-defs-3
      (ev '(let ((x 1) (y 7) (z 33))
             (define y 88)
             (define z y)
             6
             (begin (define a 5) (define w 4))
             a))
      5)
    (test 'internal-defs-4
      (ev '(let ((x 1) (y 7) (z 33))
             (define y 88)
             (define z (lambda (x) x))
             6
             (begin (define a 5) (define w (z y)))
             w))
      88)

    (test 'set-1
      (ev '(let ((x 0)) (set! x 2) x))
      2)
    (test 'set-2
      (ev '(let ((x 0)) (define y x) (set! x 2) y))
      0)

    (test 'letrec-1
      (ev '(letrec ((w (lambda () (y)))
                    (x 32)
                    (y (lambda () x)))
             (w)))
      32)
    (test 'letrec-2
      (ev '(letrec ((w (lambda () y))
                    (x 33)
                    (y x))
             (w)))
      33)

    (test 'begin-1
      (ev '(begin 1))
      1)
    (test 'begin-2
      (ev '(begin 1 2))
      2)
    (test 'begin-3
      (ev '(let ((x 1))
             (let ((y (begin (set! x 6) x)))
               y)))
      6)

    (test 'if-1
      (ev '(if #t 'yes 'no))
      'yes)
    (test 'if-2
      (ev '(if #f 'yes 'no))
      'no)
    (test 'if-3
      (ev '(if 0 'yes 'no))
      'yes)
    (test 'if-4
      (ev '(if (car '(#t . #f)) 'yes 'no))
      'yes)
    (test 'if-5
      (ev '(if (cdr '(#t . #f)) 'yes 'no))
      'no)

    (test 'and-1
      (ev '(and))
      #t)
    (test 'and-2
      (ev '(and 1))
      1)
    (test 'and-3
      (ev '(and #f 2))
      #f)
    (test 'and-4
      (ev '(and 2 3))
      3)
    (test 'and-5
      (ev '(and 2 3 4))
      4)
    (test 'and-6
      (ev '(and 2 #f 4))
      #f)

    (test 'or-1
      (ev '(or))
      #f)
    (test 'or-2
      (ev '(or 1))
      1)
    (test 'or-3
      (ev '(or #f 2))
      2)
    (test 'or-4
      (ev '(or 2 3))
      2)
    (test 'or-5
      (ev '(or #f #f 4))
      4)
    (test 'or-6
      (ev '(or 2 #f 4))
      2)

    (test 'when-1
      (ev '(when 1 2))
      2)
    (test 'when-2
      (ev '(when #f 3))
      #t)

    (test 'unless-1
      (ev '(unless 1 2))
      #t)
    (test 'unless-2
      (ev '(unless #f 3))
      3)

    (test 'cond-1
      (ev '(cond (1 2)))
      2)
    (test 'cond-2
      (ev '(cond (#f 3)
                 (4 5)))
      5)

    (test 'misc-1
      (ev '((lambda (w #f x #f y . z)
              (if (x y z '(a ... z))
                'true
                'false))
            1 2 (lambda x x) 4 5 6 7))
      'true)
    (test 'misc-2
      (ev '((lambda (w #f x #f y . z)
              (if (x y z '(a ... z))
                'true
                'false))
            1 2 (lambda x #f) 4 5 6 7))
      'false)
    (test 'misc-3
      (ev '((lambda (w #f x #f y . z)
              (if 'true
                (x y z '(a ... z))
                'false))
            1 2 (lambda x x) 4 5 6 7))
      '(5 (6 7) (a ... z)))

    (test 'let*-1
      (ev '(let* ((a 1) (b (cons 2 a))) b))
      '(2 . 1))
    (test 'let*-2
      (ev '(let* ((a 1) (b (cons 2 a))) b))
      (let* ((a 1) (b (cons 2 a))) b))

    (test 'shift/reset-1
      (ev '(* 2 (reset (+ 1 (shift k (k 5))))))
      12)
    (test 'shift/reset-2
      (ev '(reset (* 2 (shift k (k (k 4))))))
      16)

    (test 'fix-0
      (ev '(let ((list (lambda xs xs))
                 (fix (lambda (f)
                        ((lambda (d) (d d))
                         (lambda (x) (f (lambda (a b) ((x x) a b))))))))
             (let ((append
                     (fix (lambda (append)
                            (lambda (xs ys)
                              (if (null? xs)
                                ys
                                (cons (car xs) (append (cdr xs) ys))))))))
               (list (append '() '())
                     (append '(foo) '(bar))
                     (append '(1 2) '(3 4))))))
      '(() (foo bar) (1 2 3 4)))

    (test 'fix-1
      (ev '(let ((list (lambda xs xs))
                 (fix (lambda (f)
                        ((lambda (d) (d d))
                         (lambda (x) (f (lambda a (apply (x x) a))))))))
             (let ((append
                     (fix (lambda (append)
                            (lambda (xs ys)
                              (if (null? xs)
                                ys
                                (cons (car xs) (append (cdr xs) ys))))))))
               (list (append '() '())
                     (append '(foo) '(bar))
                     (append '(1 2) '(3 4))))))
      '(() (foo bar) (1 2 3 4)))

    (test 'fix-2
      (ev '(let ((list (lambda xs xs))
                 (fix (lambda (f)
                        ((lambda (d) (d d))
                         (lambda (x) (f (lambda a (apply (x x) a))))))))
             (let ((append
                     (fix (lambda (append)
                            (lambda (xs ys)
                              (if (null? xs)
                                ys
                                (cons (car xs) (append (cdr xs) ys))))))))
               (list (append '() '())
                     (append '(foo) '(bar))
                     (append '(1 2) '(3 4))))))
      (let ((list (lambda xs xs))
            (fix (lambda (f)
                   ((lambda (d) (d d))
                    (lambda (x) (f (lambda a (apply (x x) a))))))))
        (let ((append
                (fix (lambda (append)
                       (lambda (xs ys)
                         (if (null? xs)
                           ys
                           (cons (car xs) (append (cdr xs) ys))))))))
          (list (append '() '())
                (append '(foo) '(bar))
                (append '(1 2) '(3 4))))))

    (test 'fix*-1
      (ev '(let ((list (lambda xs xs))
                 (fix (lambda (f)
                        ((lambda (d) (d d))
                         (lambda (x) (f (lambda a (apply (x x) a))))))))
             (let ((map (fix (lambda (map)
                               (lambda (f xs)
                                 (if (null? xs)
                                   '()
                                   (cons (f (car xs)) (map f (cdr xs)))))))))
               (let ((fix*
                       (fix (lambda (fix*)
                              (lambda fs
                                (map (lambda (fi)
                                       (lambda a
                                         (apply (apply fi (apply fix* fs)) a)))
                                     fs))))))
                 (let ((even&odd
                         (fix* (lambda (even? odd?)
                                 (lambda (n) (if (null? n)
                                               #t
                                               (odd? (cdr n)))))
                               (lambda (even? odd?)
                                 (lambda (n)
                                   (if (null? n)
                                     #f
                                     (even? (cdr n))))))))
                   (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
                     (list (even? '())    (odd? '())
                           (even? '(s))   (odd? '(s))
                           (even? '(s s)) (odd? '(s s)))))))))
      '(#t #f #f #t #t #f))

    (test 'fix*-2
      (ev '(let ((list (lambda xs xs))
                 (fix (lambda (f)
                        ((lambda (d) (d d))
                         (lambda (x) (f (lambda a (apply (x x) a))))))))
             (let ((map (fix (lambda (map)
                               (lambda (f xs)
                                 (if (null? xs)
                                   '()
                                   (cons (f (car xs)) (map f (cdr xs)))))))))
               (let ((fix*
                       (fix (lambda (fix*)
                              (lambda fs
                                (map (lambda (fi)
                                       (lambda a
                                         (apply (apply fi (apply fix* fs)) a)))
                                     fs))))))
                 (let ((even&odd
                         (fix* (lambda (even? odd?)
                                 (lambda (n) (if (null? n)
                                               #t
                                               (odd? (cdr n)))))
                               (lambda (even? odd?)
                                 (lambda (n)
                                   (if (null? n)
                                     #f
                                     (even? (cdr n))))))))
                   (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
                     (list (even? '())    (odd? '())
                           (even? '(s))   (odd? '(s))
                           (even? '(s s)) (odd? '(s s)))))))))
      (let ((list (lambda xs xs))
            (fix (lambda (f)
                   ((lambda (d) (d d))
                    (lambda (x) (f (lambda a (apply (x x) a))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f xs)
                            (if (null? xs)
                              '()
                              (cons (f (car xs)) (map f (cdr xs)))))))))
          (let ((fix*
                  (fix (lambda (fix*)
                         (lambda fs
                           (map (lambda (fi)
                                  (lambda a
                                    (apply (apply fi (apply fix* fs)) a)))
                                fs))))))
            (let ((even&odd
                    (fix* (lambda (even? odd?)
                            (lambda (n) (if (null? n)
                                          #t
                                          (odd? (cdr n)))))
                          (lambda (even? odd?)
                            (lambda (n)
                              (if (null? n)
                                #f
                                (even? (cdr n))))))))
              (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
                (list (even? '())    (odd? '())
                      (even? '(s))   (odd? '(s))
                      (even? '(s s)) (odd? '(s s)))))))))

    (test 'vector-1
      (map ev '((vector)
                (vector 3 1)))
      '(#() #(3 1)))
    (test 'vector-2
      (map ev '((vector-length (vector))
                (vector-length (vector 3 1))))
      '(0 2))
    (test 'vector-3
      (map ev '((vector-ref (vector 5) 0)
                (vector-ref (vector 3 1 2) 0)
                (vector-ref (vector 3 1 2) 1)))
      '(5 3 1))
    (test 'vector-4
      (map ev '((vector? (vector))
                (vector? (vector 3 1))
                (vector? '(x x))
                (vector? (lambda x x))))
      '(#t #t #f #f))
    (test 'vector-5
      (map ev '((vector-ref '#(1 2 3) 0)
                (vector-ref '#(4 5 6) 2)
                (vector-ref '#(7 8 9) 1)
                (vector-ref (car (cdr (list 6 (vector 7 (cons 8 9) 0) 1))) 1)
                (vector-ref (car (cdr (list 6 (vector
                                                7 (cons 8 9) 0 (car '(5 . #f)))
                                            1))) 3)))
      '(1 6 8 (8 . 9) 5))

    (test 'list-1
      (map ev '((list)
                (list 6 7)))
      '(() (6 7)))
    (test 'list-2
      (map ev '((list? (list))
                (list? (list 6 7))
                (list? '(6 . 7))))
      '(#t #t #f))

    (test 'pair-4
      (ev '(pair? (vector 3 1 2)))
      #f)
    (test 'procedure-5
      (ev '(procedure? (vector 3 1 2)))
      #f)

    (test 'equal-1
      (ev '(map equal?
                '(9 9 () #t #f one two)
                '(9 8 () #t #t one one)))
      '(#t #f #t #t #f #t #f))
    (test 'equal-2
      (ev '(map equal?
                '((5 6) (4 6) (5 7) #(1 2 3) #(1 8 3))
                '((5 6) (5 6) (5 6) #(1 2 3) #(1 2 3))))
      '(#t #f #f #t #f))
    (test 'equal-3
      (ev '(let ((id (lambda (x) x)))
             (map equal? (list id id) (list id (lambda (x) x)))))
      '(#t #f))))

(require
  "interop.rkt"
  (prefix-in nscm: "io.rkt")
  'stage
  racket/list
  racket/match
  racket/runtime-path)

(define (test/report)
  (define tests-total 0)
  (define test-failures '())
  (define (test-report)
    (define tests-failed (length test-failures))
    (define tests-passed (- tests-total tests-failed))
    (printf "********************************\nTests passed: ~a out of ~a\n"
            tests-passed tests-total)
    (unless (= tests-passed tests-total)
      (printf "Tests failed: ~a out of ~a\n" tests-failed tests-total)
      (printf "~s\n" test-failures)))
  (define (test name actual expected)
    (printf "Testing ~a: " name)
    (set! tests-total (+ tests-total 1))
    (cond ((equal? expected actual) (printf "Succeeded.\n"))
          (else (printf "Failed.\nExpected: ~s\nActual: ~s\n****************\n"
                        expected actual)
                (set! test-failures (cons name test-failures)))))
  (cons test test-report))

(define (module-apply m ns)
  (define pro ($apply (vector-ref m 2) (alist-ref* ns (vector-ref m 0))))
  (if (vector-ref m 1) (map cons (vector-ref m 1) pro) pro))
(define (namespace-link* ns m*)
  (foldl (lambda (m ns) (append (module-apply m ns) ns)) ns m*))
(define (premodule:parse form)
  (define (i->r items rrns)
    (foldl (lambda (item rrns)
             (if (and (pair? item) (equal? (car item) 'rename))
               (append (reverse (cdr item)) rrns)
               (cons (list item item) rrns))) rrns items))
  (let loop ((head (car form)) (rrequired '()) (rprovided '()))
    (define next (and (pair? head) (car head)))
    (cond ((null? head)
           (define rd (reverse rrequired)) (define pd (reverse rprovided))
           (define required (map car rd)) (define required-priv (map cadr rd))
           (define provided (map cadr pd)) (define provided-priv (map car pd))
           (vector required provided required-priv provided-priv (cdr form)))
          ((equal? (car next) 'require)
           (loop (cdr head) (i->r (cdr next) rrequired) rprovided))
          ((equal? (car next) 'provide)
           (loop (cdr head) rrequired (i->r (cdr next) rprovided)))
          ((equal? (car next) 'language)  ;; Ignore and assume base language.
           (loop (cdr head) rrequired rprovided))
          (#t (error "invalid module header:" (car form) head)))))

(define (nscm:module body)
  (define ($list _) (stage env:initial (s->ns '(lambda xs xs))))
  (define pm (premodule:parse body))
  (define code (append (list 'lambda (vector-ref pm 2))
                       (append (vector-ref pm 4)
                               (list (cons $list (vector-ref pm 3))))))
  (vector (vector-ref pm 0) (vector-ref pm 1) (base:eval (s->ns code))))

(define (build-nscheme-namespace)
  (define (libmod name)
    (define file-name (string-append "lib/" (symbol->string name) ".scm"))
    (nscm:module (read*/file file-name)))
  (define modules '(common ast parse module base eval extended io
                           backend-racket module-racket))
  (define test-modules '(base-test extended-test))
  (define t/r (test/report))
  (printf "~a\n" "Testing nscheme library:")
  (define ns:init `((test         . ,(lift (car t/r)))
                    (printf       . ,(lift nscm:printf))
                    (file-exists? . ,(lift nscm:file-exists?))
                    (eof?         . #f)
                    (read         . #f)
                    (read*/string . #f)
                    (read*/file   . ,(lift nscm:read*/file))
                    (write        . #f)
                    (write/file   . ,(lift nscm:write/file))))
  (define ns:nscheme (namespace-link* ns:init (map libmod modules)))
  (namespace-link* ns:nscheme (map libmod test-modules))
  ((cdr t/r))
  ns:nscheme)

(module+ test
  (printf "~a\n" "Testing bootstrap.rkt stage:")
  (define t/r (test/report))
  (stage:test! (car t/r))
  ((cdr t/r))
  (void (build-nscheme-namespace)))

(module+ main
  (define-runtime-path here ".")
  (define program-path (build-path here "compile.scm"))
  (define simple-path (path->string (simplify-path program-path)))
  (define str:in
    (path->string (simplify-path (build-path here "compile.scm"))))
  (define ns:nscheme (build-nscheme-namespace))
  (define nscm:ns:nscheme
    (map (lambda (r) (cons (symbol->string (car r)) (cdr r))) ns:nscheme))
  (define ns:full
    (append `((program-arguments . (,(path:s->ns simple-path) ,str:in))
              (ns:nscheme        . ,nscm:ns:nscheme))
            ns:nscheme))
  (define build-rkt-nscheme.scm (read*/file program-path))
  (time (void (module-apply (nscm:module build-rkt-nscheme.scm) ns:full))))
