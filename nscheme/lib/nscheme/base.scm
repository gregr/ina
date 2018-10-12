(provide lang:base stage)

(require length=? length>=? ctx:var ctx:set! ctx:op ctx:def
         env:empty env-ref env-ref-prop
         param? bpair*?! ncons param-map param-names
         ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:reset ast:shift ast:prim primitive-op-descriptions)

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
(define (ast:let p* v* body)  (ast:apply* (ast:lambda p* body) v*))
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
      (@let env (list (car b*)) (lambda (env) (loop (cdr b*) env))))))
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

(define (defst:empty env) (vector env '()))
(define (defst-env st)    (vector-ref st 0))
(define (defst-rdef* st)  (vector-ref st 1))
(define (@@define st n def-body)
  (vector (defst-env st) (cons (cons n def-body) (defst-rdef* st))))
;; TODO: $define-syntax ?
(define (@definition* st form*)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($define (and n (env-ref-prop (defst-env st) n ctx:def #f))))
             (cond ($define ($define st (cdr form)))
                   (#t (@@define st #f form))))) st form*))
(define (@begin/define st tail)
  (cond ((list? tail) (@definition* st tail))
        (#t           (error '"invalid begin:" tail))))
(define (@define st tail)
  (define (@ i) (list-ref tail i)) (define (@. i) (list-tail tail i))
  (cond ((and (length=? 2 tail) (param? (@ 0))) (@@define st (@ 0) (@ 1)))
        ((and (length>=? 2 tail) (pair? (@ 0)) (string? (car (@ 0))))
         (define (edef env) (apply @lambda env (cdr (@ 0)) (@. 1)))
         (@@define st (car (@ 0)) edef))
        (#t (error '"invalid define:" tail))))

(define (@body* env body*)
  (unless (list? body*) (error '"body must be a list:" body*))
  (define rdef* (defst-rdef* (@definition* (defst:empty env) body*)))
  (when (null? rdef*) (error '"body cannot be empty:" body*))
  (define p?e* (reverse (cdr rdef*)))
  (define final-def (car rdef*))
  (define body-final (cdr final-def))
  (when (car final-def) (error '"body cannot end with a definition:" body*))
  (if (null? p?e*) (stage env body-final)
    (@letrec env (map (lambda (b) (list (car b) (cdr b))) p?e*) body-final)))

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
    ctx:def (list (cons 'begin @begin/define) (cons 'define @define))))
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
                    (lambda (env) (let ((env:base (env-freeze env)))
                                    (stage env:base (shift k k))))))))
(define lang:base (reset (lang:primitive base-code)))
