#lang racket/base
(provide eval env:base s->ns ns->s plift)
(require "common.rkt" racket/bool racket/control racket/list)

(define type-predicates
  (list (cons 'procedure? procedure?)
        (cons 'mvector?   mvector?)
        (cons 'vector?    vector?)
        (cons 'pair?      pair?)
        (cons 'null?      null?)
        (cons 'boolean?   boolean?)
        (cons 'string?    string?)
        (cons 'number?    number?)
        (cons 'integer?   integer?)
        (cons 'fixnum?    fixnum?)
        (cons 'flonum?    flonum?)))

(define primitive-op-descriptions
  (append (map (lambda (tp) (cons (car tp) '((#f) boolean?))) type-predicates)
          '((boolean=?   (boolean? boolean?)     boolean?)
            (number=?    (number? number?)       boolean?)
            (string=?    (string? string?)       boolean?)
            (mvector=?   (mvector? mvector?)     boolean?)
            (procedure=? (procedure? procedure?) boolean?)

            (string->vector (string?)  vector?)
            (vector->string (vector?)  string?)

            (cons (#f #f) pair?)
            (car  (pair?) #f)
            (cdr  (pair?) #f)

            (vector-ref    (vector? fixnum?) #f)
            (vector-length (vector?)         fixnum?)

            (make-mvector    (fixnum? #f)          mvector?)
            (mvector->vector (mvector?)            vector?)
            (mvector-set!    (mvector? fixnum? #f) #t)
            (mvector-ref     (mvector? fixnum?)    #f)
            (mvector-length  (mvector?)            fixnum?)

            ;; TODO: derive these.
            (string<? (string? string?) boolean?)
            (string>? (string? string?) boolean?)
            ;; TODO: flonum variants.
            (=  (number? number?) boolean?)
            (<= (number? number?) boolean?)
            (<  (number? number?) boolean?)
            (>= (number? number?) boolean?)
            (>  (number? number?) boolean?)
            (+  (number? number?) number?)
            (*  (number? number?) number?)
            (-  (number? number?) number?)
            (/  (number? number?) number?)

            ;bitwise-and
            ;bitwise-ior
            ;bitwise-xor
            ;bitwise-not
            ;bitwise-bit-set?
            ;bitwise-bit-field
            ;arithmetic-shift
            ;integer-length

            ;round
            ;quotient
            ;remainder
            )))

(define (ast:quote datum)       (vector 'quote  datum))
(define (ast:var address)       (vector 'var    address))
(define (ast:set! param arg)    (vector 'set!   param arg))
(define (ast:if c t f)          (vector 'if     c t f))
(define (ast:apply proc arg)    (vector 'apply  proc arg))
(define (ast:reset body)        (vector 'reset  body))
(define (ast:shift proc)        (vector 'shift  proc))
(define (ast:lambda param body) (vector 'lambda param body))
(define (ast:prim name a*)      (vector 'prim   name a*))

;; Primitive operations
(define primitive-op-handlers
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
        (cons '/               /)))

(unless (= (length primitive-op-descriptions) (length primitive-op-handlers))
  (error '"mismatching primitive op handlers:"
         (map car primitive-op-handlers) (map car primitive-op-descriptions)))

(define primitive-ops
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arg-sig (cadr po-desc))
         (define return-sig (caddr po-desc))  ;; TODO: validate return type?
         (define op (cdr (assoc name primitive-op-handlers)))
         (define (valid? a*)
           (andmap (lambda (ty? a)
                     (or (not ty?) ((cdr (assoc ty? type-predicates)) a)))
                   arg-sig a*))
         (define (full-op a*)
           (if (valid? a*) (apply op a*)
             (error '"primitive op type error:" name arg-sig a*)))
         (cons name full-op)) primitive-op-descriptions))

(define (ast-eval ast)
  ;; Runtime environments
  (define (env-extend* env b*)
    (foldl (lambda (b e)
             (define cell    (make-mvector 1 (cdr b)))
             (define (get)   (mvector-ref  cell 0))
             (define (set v) (mvector-set! cell 0 v))
             (cons (cons (car b) (cons get set)) e)) env b*))
  (define (env-ref-capabilities env addr)
    (define rib (assoc addr env))
    (if rib (cdr rib) (error '"unbound address:" addr)))
  (define (env-ref env addr)    ((car (env-ref-capabilities env addr))))
  (define (env-set! env addr v) ((cdr (env-ref-capabilities env addr)) v))
  ((let ev ((ast ast))
     (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
     (if (procedure? ast) ast
       (cond ((? 'quote)  (let ((datum (@ 1))) (lambda (env) datum)))
             ((? 'var)    (let ((n (@ 1)))     (lambda (env) (env-ref env n))))
             ((? 'set!)   (let ((param (@ 1)) (arg (ev (@ 2))))
                            (lambda (env)
                              (define (! b) (env-set! env (car b) (cdr b)))
                              (for-each ! (param-bind param (arg env))))))
             ((? 'if)     (let ((c (ev (@ 1))) (t (ev (@ 2))) (f (ev (@ 3))))
                            (lambda (env) (if (c env) (t env) (f env)))))
             ((? 'apply)  (let ((proc (ev (@ 1))) (arg (ev (@ 2))))
                            (lambda (env) ($apply (proc env) (arg env)))))
             ((? 'lambda) (let ((param (@ 1)) (body (ev (@ 2))))
                            (lambda (env) (lambda (arg)
                                            (define b* (param-bind param arg))
                                            (body (env-extend* env b*))))))
             ((? 'reset)  (let ((body (ev (@ 1))))
                            (lambda (env) (reset (body env)))))
             ((? 'shift)  (let ((proc (ev (@ 1))))
                            (lambda (env)
                              (shift k ($apply (proc env) (list (plift k)))))))
             ((? 'prim)   (let ((name (@ 1)) (a* (map ev (@ 2))))
                            (define op (or (alist-ref primitive-ops name #f)
                                           (error '"invalid primitive:" name)))
                            (lambda (env) (op (map (lambda (a) (a env)) a*)))))
             (#t          (error '"unknown ast:" ast))))) '()))

(define (env-extend*/var env n*)
  (define (bind n) (cons n (list (cons ctx:var n) (cons ctx:set! n))))
  (append (map bind n*) env))
(define (env-extend*/syntax env ctx b*)
  (define (bind b) (let ((n (car b)))
                     (cons n (cons (cons ctx (cdr b)) (env-ref env n)))))
  (append (map bind b*) (alist-remove* env (map car b*))))
(define (env-freeze env)
  (map (lambda (b) (cons (car b) (alist-remove* (cdr b) (list ctx:set!))))
       env))

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

;; Staging
(define (stage env form)
  (cond ((pair? form)
         (let ((p (car form)) (a* (cdr form)))
           (let ((op (and (string? p) (env-ref-prop env p ctx:op #f))))
             (if op (apply op env a*)
               (ast:apply* (stage env p) (stage* env a*))))))
        ((string? form) (ast:var (or (env-ref-prop env form ctx:var #f)
                                     (error '"unbound variable:" form))))
        ((or (boolean? form) (number? form)) (ast:quote form))
        ((procedure? form)                   (form env))
        ((symbol? form)                      (stage env (symbol->string form)))
        (#t                                  (error '"invalid syntax:" form))))
(define (stage* env form*) (map (lambda (f) (stage env f)) form*))

(define (@apply env proc arg) (ast:apply (stage env proc) (stage env arg)))
(define (@quote env datum)    (ast:quote datum))
(define (@reset env . body)   (ast:reset (@body* env body)))
(define (@shift env k . body) (ast:shift (apply @lambda env (list k) body)))
(define (@set! env param arg)
  (define (setter n) (or (env-ref-prop env n ctx:set! #f)
                         (error '"cannot set! variable:" n)))
  (ast:set! (param-map setter param) (stage env arg)))
(define (@if env c t f) (ast:if (stage env c) (stage env t) (stage env f)))
(define (@lambda env param . body)
  (let* ((n* (param-names param)) (env (alist-remove* env n*)))
    (ast:lambda param (@body* (env-extend*/var env n*) body))))
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
           (@let env (list (list '"temp" arg))
                 (lambda (env) (@if env '"temp" '"temp" (lambda (_) rest)))))
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
    (ast:let names (map (lambda (_) ast:true) names)
             (ast:begin (map (lambda (act) (act env)) actions)))))
(define (@begin/define st . forms)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($def (and n (env-ref-prop (defstate-env st) n ctx:def #f))))
             (if $def (apply $def st (cdr form))
               (defstate-actions-add-expr st form)))) st forms))
(define (@def st param arg)
  (define names (param-names param))
  (define env (env-extend*/var (alist-remove* (defstate-env st) names) names))
  (defstate-actions-add (defstate-env-set (defstate-names-add st names) env)
                        (lambda (env) (@set! env param arg))))
(define (@define st param . body)
  (if (pair? param)
    (@define st (car param)
             (lambda (env) (apply @lambda env (cdr param) body)))
    (apply @def st param body)))
(define (@body* env body*)
  (defstate-run (apply @begin/define (defstate:empty env) body*)))

;; Primitive language definition
(define (stager:primitive-syntax name proc arity exact?)
  (cons name (lambda (env . tail)
               (unless ((if exact? length=? length>=?) arity tail)
                 (error '"invalid syntax arity:" arity (cons name tail)))
               (apply proc env tail))))
(define primitive-syntax-bindings
  (append
    (map (lambda (desc) (apply stager:primitive-syntax desc))
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
               (list 'unless @unless 1 #f)))
    (map (lambda (po-desc)
           (cons (car po-desc)
                 (lambda (env . tail)
                   (unless (length=? (length (cadr po-desc)) tail)
                     (error '"invalid primitive op:" po-desc tail))
                   (ast:prim (car po-desc) (stage* env tail)))))
         primitive-op-descriptions)))

(define env:primitive
  (env-extend*/syntax
    (env-extend*/syntax env:empty ctx:op primitive-syntax-bindings)
    ctx:def (list (cons 'begin  @begin/define)
                  (cons 'define @define)
                  (cons 'def    @def))))
(define (lang:primitive form) (stage env:primitive form))

;; Base language definition
(define primitive-op-procs
  (map (lambda (po-desc)
         (define (x i) (vector-ref '#(x0 x1 x2 x3 x4) i))
         (define p* (map x (range (length (cadr po-desc)))))
         (list (car po-desc) (list 'lambda p* (cons (car po-desc) p*))))
       primitive-op-descriptions))

(define derived-op-procs
  '((error (lambda args ('error args)))
    (not (lambda (b) (if b #f #t)))
    (caar (lambda (v) (car (car v))))
    (cadr  (lambda (xs) (car (cdr xs))))
    (cadar (lambda (v) (cadr (car v))))
    (caddr (lambda (xs) (cadr (cdr xs))))
    (list-tail (lambda (xs i) (if (= 0 i) xs (list-tail (cdr xs) (- i 1)))))
    (list-ref  (lambda (xs i) (car (list-tail xs i))))
    (list->vector (lambda (xs)
                    (define result (make-mvector (length xs) #t))
                    (foldl (lambda (x i) (mvector-set! result i x) (+ i 1))
                           0 xs)
                    (mvector->vector result)))
    (vector->list (lambda (v)
                    (let loop ((i (- (vector-length v) 1)) (xs '()))
                      (if (< i 0) xs
                        (loop (- i 1) (cons (vector-ref v i) xs))))))
    (equal? (lambda (a b)
              (cond ((pair? a)   (and (pair? b) (equal? (car a) (car b))
                                      (equal? (cdr a) (cdr b))))
                    ((vector? a) (and (vector? b) (equal? (vector->list a)
                                                          (vector->list b))))
                    ((boolean? a)   (and (boolean? b)   (boolean=? a b)))
                    ((string?  a)   (and (string?  b)   (string=?  a b)))
                    ((number?  a)   (and (number?  b)   (number=?  a b)))
                    ((mvector? a)   (and (mvector? b)   (mvector=? a b)))
                    ((procedure? a) (and (procedure? b) (procedure=? a b)))
                    ((null? a)      (null? b)))))
    (vector (lambda xs (list->vector xs)))
    (list?  (lambda (v) (or (and (pair? v) (list? (cdr v))) (null? v))))
    (list   (lambda xs xs))
    (list*  (lambda (x . xs) (if (null? xs) x (cons x (apply list* xs)))))
    (foldl  (lambda (f acc xs) (if (null? xs) acc
                                 (foldl f (f (car xs) acc) (cdr xs)))))
    (foldr  (lambda (f acc xs) (if (null? xs) acc
                                 (f (car xs) (foldr f acc (cdr xs))))))
    (map (lambda (f xs . xss)
           (define (map1 f xs) (if (null? xs) '()
                                 (cons (f (car xs)) (map1 f (cdr xs)))))
           (cond ((null? xs) '())
                 (#t (cons (apply f (car xs) (map1 car xss))
                           (apply map f (cdr xs) (map1 cdr xss)))))))
    (for-each (lambda args (apply map args) #t))
    (andmap (lambda (f xs . xss)
              (let loop ((last #t) (xs xs) (xss xss))
                (and last (if (null? xs) last
                            (loop (apply f (car xs) (map car xss))
                                  (cdr xs) (map cdr xss)))))))
    (ormap (lambda (f xs . xss)
             (cond ((null? xs) #f)
                   ((apply f (car xs) (map car xss)))
                   (#t (apply ormap f (cdr xs) (map cdr xss))))))
    (filter (lambda (p? xs)
              (cond ((null? xs) '())
                    ((p? (car xs)) (cons (car xs) (filter p? (cdr xs))))
                    (#t (filter p? (cdr xs))))))
    (filter-not (lambda (p? xs) (filter (lambda (x) (not (p? x))) xs)))
    (remf (lambda (p? xs)
            (cond ((null? xs)    '())
                  ((p? (car xs)) (cdr xs))
                  (#t (cons (car xs) (remf p? (cdr xs)))))))
    (remove (lambda (v xs) (remf (lambda (x) (equal? x v)) xs)))
    (length (lambda (xs) (foldl (lambda (_ l) (+ 1 l)) 0 xs)))
    (append (lambda xss (foldr (lambda (xs yss) (foldr cons yss xs)) '() xss)))
    (reverse-append (lambda (xs ys) (foldl cons ys xs)))
    (reverse (lambda (xs) (reverse-append xs '())))
    (range (lambda (n)
             (let loop ((i 0)) (if (= i n) '() (cons i (loop (+ i 1)))))))
    (take (lambda (xs n) (if (= 0 n) '()
                           (cons (car xs) (take (cdr xs) (- n 1))))))
    (drop (lambda (xs n) (if (= 0 n) xs (drop (cdr xs) (- n 1)))))
    (memf (lambda (? xs) (cond ((null? xs) #f)
                               ((? (car xs)) xs)
                               (#t (memf ? (cdr xs))))))
    (member (lambda (v xs) (memf (lambda (x) (equal? x v)) xs)))
    (assoc (lambda (k xs) (cond ((null? xs) #f)
                                ((equal? k (caar xs)) (car xs))
                                (#t (assoc k (cdr xs))))))
    (alist-ref (lambda (rs key default) (let ((rib (assoc key rs)))
                                          (if rib (cdr rib) default))))
    (alist-remove* (lambda (rs keys)
                     (filter (lambda (rib) (not (member (car rib) keys))) rs)))
    (string-append (lambda ss
                     (define css (map vector->list (map string->vector ss)))
                     (vector->string (list->vector (apply append css)))))
    ))

(define base-code
  (list 'let primitive-op-procs
        (list 'let '((apply (lambda (f arg . args)
                              (define (cons* x xs)
                                (if (null? xs) x
                                  (cons x (cons* (car xs) (cdr xs)))))
                              (apply f (cons* arg args)))))
              (list 'letrec derived-op-procs
                    (lambda (env) (let ((env (env-freeze env)))
                                    (stage env (shift k k))))))))
(define lang:base (reset (lang:primitive base-code)))

(define env:base '())

(define (eval env form) (ast-eval (lang:base form)))

;; Tests:
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

(define (ev code) (eval env:base code))

(test 'lambda-1  ;; Formal parameter lists are generalized to arbitrary trees.
  (ev '((lambda (() a (b)) (cons a b))
        '() 1 '(2)))
  '(1 . 2))
(test 'lambda-2  ;; Formal parameter lists are generalized to arbitrary trees.
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
  (ev '(let ((op (lambda (x) (car x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
         (if (op datum) ta fa)))
  'yes)
(test 'let-7
  (ev '(let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
         (if (op datum) ta fa)))
  'no)
(test 'let-8
  (ev '(let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
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
(test 'cond-3
  (ev '(cond (#f 3)
             (8)
             (4 5)))
  8)

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
                                            7 (cons 8 9) 0 (car '(10 . #f)))
                                        1))) 3)))
  '(1 6 8 (8 . 9) 10))

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
  '(#t #f))


(test-report)
