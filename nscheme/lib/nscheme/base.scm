(provide lang:base parse env-reify)

(require ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:reset ast:shift ast:prim primitive-op-descriptions)

;; Association lists
(define (assoc-ref xs k default) (let ((kv (assoc k xs)))
                                   (or (and kv (cdr kv)) default)))
(define (assoc-remove xs k) (remf (lambda (x) (equal? (car x) k)) xs))
(define (assoc-extend* xs b*)
  (append b* (foldl (lambda (k xs) (assoc-remove xs k)) xs (map car b*))))

;; Pattern matching
(define (length=? len xs)  (and (list? xs) (= (length xs) len)))
(define (length>=? len xs) (and (list? xs) (>= (length xs) len)))

;; Syntactic environments
(define (name? n) (string? n))
(define ctx:var  'variable)
(define ctx:set! 'set!)
(define ctx:op   'operator)
(define ctx:def  'definition)
(define env:empty                      '())
(define (env-ref env n)                (assoc-ref env n '()))
(define (env-ref-prop env n k default) (assoc-ref (env-ref env n) k default))
(define (env-extend*/var env p*)
  (define (bind n) (cons n (list (cons ctx:var ast:var)
                                 (cons ctx:set! ast:set!))))
  (assoc-extend* env (map bind (filter (lambda (p?) p?) p*))))
(define (env-extend*/syntax env ctx b*)
  (define (bind b) (let ((n (car b)))
                     (cons n (cons (cons ctx (cdr b)) (env-ref env n)))))
  (assoc-extend* env (map bind b*)))
(define (env-freeze env)
  (map (lambda (b) (cons (car b) (assoc-remove (cdr b) ctx:set!))) env))

;; Parameters
(define (param? p) (or (not p) (name? p)))
(define (bpair*?! b*)
  (define (? b) (and (length=? 2 b) (param? (car b))))
  (unless (and (list? b*) (andmap ? b*)) (error '"invalid binding list:" b*)))

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
           (ast:begin (map ! (range (length xs)) xs)
                      (ast:prim 'mvector->vector (list $mv)))))
(define (ast:apply* $proc $a*) (ast:apply $proc (apply ast:list $a*)))
(define (ast:let p?* v* body)  (ast:apply* (ast:lambda p?* body) v*))
(define (ast:begin body-initial* body-final)
  (cond ((null? body-initial*) body-final)
        (#t (ast:let '(#f) (list (car body-initial*))
                     (ast:begin (cdr body-initial*) body-final)))))

;; Parsing
(define (parse env form)
  (let loop ((form form))
    (cond ((or (boolean? form) (number? form)) (ast:quote form))
          ((parse:op? env form))
          ((procedure? form)  (form env))
          ((name? form)       (parse:var env form))
          ((length>=? 1 form) (parse:apply* env (car form) (cdr form)))
          (#t                 (error '"invalid syntax:" form)))))
(define (parse* env form*) (map (lambda (f) (parse env f)) form*))

(define (parse:var env n)     ((or (env-ref-prop env n ctx:var #f)
                                   (error '"unbound variable:" n)) n))
(define (parse:set! env n $v) ((or (env-ref-prop env n ctx:set! #f)
                                   (error '"cannot set! variable:" n)) n $v))
(define (parse:op? env form)
  (let* ((n (and (pair? form) (car form)))
         (b (and (name? n) (env-ref-prop env n ctx:op #f))))
    (and b (b env form))))
(define (parse:def? st form)
  (let* ((n (and (pair? form) (car form)))
         (b (and (name? n) (env-ref-prop (defst-env st) n ctx:def #f))))
    (and b (b st form))))

(define (parse:apply* env p a*) (ast:apply* (parse env p) (parse* env a*)))
(define (parse:lambda env ~ps body*)
  (let loop ((~p?* ~ps) (p* '()))
    (define (valid?! p)
      (unless (param? p)          (error '"invalid parameters:" ~ps))
      (when (and p (member p p*)) (error '"duplicate parameters:" ~ps)))
    (cond ((pair? ~p?*)       (define p (car ~p?*)) (valid?! p)
                              (loop (cdr ~p?*)  (if p (cons p p*) p*)))
          ((not (null? ~p?*)) (loop (list ~p?*) p*))
          (#t (ast:lambda ~ps (parse:body* (env-extend*/var env p*) body*))))))
(define (parse:let env p?* e* body*)
  (ast:apply* (parse:lambda env p?* body*) (parse* env e*)))
(define (parse:letrec env p?* e* body*)
  (define (continue env)
    (ast:begin (map (lambda (p v) (if p (ast:set! p v) v)) p?* (parse* env e*))
               (parse:let env p?* p?* body*)))
  (parse:let env p?* (map (lambda (_) #t) p?*) (list continue)))

(define (defst:empty env) (vector env '()))
(define (defst-env st)    (vector-ref st 0))
(define (defst-rdef* st)  (vector-ref st 1))
(define ($define st n def-body)
  (vector (defst-env st) (cons (cons n def-body) (defst-rdef* st))))
;; TODO: $define-syntax ?
(define (parse:definition* st form*)
  (foldl (lambda (form st) (cond ((parse:def? st form))
                                 (#t ($define st #f form)))) st form*))
(define (parse:begin st form)
  (cond ((length>=? 1 form) (parse:definition* st (list-tail form 1)))
        (#t                 (error '"invalid begin:" form))))
(define (parse:define st form)
  (define (@ i) (list-ref form i)) (define (@. i) (list-tail form i))
  (cond ((and (length=? 3 form) (param? (@ 1))) ($define st (@ 1) (@ 2)))
        ((and (length>=? 3 form) (pair? (@ 1)) (name? (car (@ 1))))
         (define (edef env) (parse:lambda env (cdr (@ 1)) (@. 2)))
         ($define st (car (@ 1)) edef))
        (#t (error '"invalid define:" form))))

(define (parse:body* env body*)
  (unless (list? body*) (error '"body must be a list:" body*))
  (define rdef* (defst-rdef* (parse:definition* (defst:empty env) body*)))
  (when (null? rdef*) (error '"body cannot be empty:" body*))
  (define p?e* (reverse (cdr rdef*)))
  (define final-def (car rdef*))
  (define body-final (cdr final-def))
  (when (car final-def) (error '"body cannot end with a definition:" body*))
  (if (null? p?e*) (parse env body-final)
    (parse:letrec env (map car p?e*) (map cdr p?e*) (list body-final))))
(define (parse:and* env e*)
  (define re* (reverse e*))
  (if (null? re*) ast:true
    (foldr (lambda (e r) (ast:if (parse env e) r ast:false))
           (parse env (car re*)) (reverse (cdr re*)))))
(define (parse:or2 env e $rest)
  (define body (let (($tmp (ast:var 'tmp))) (ast:if $tmp $tmp $rest)))
  (ast:apply* (ast:lambda (list 'tmp) body) (list (parse env e))))

(define (env-reify env)
  (define (cons-caps b renv)
    (define b@ (assoc-ref (cdr b) ctx:var #f))
    (define b! (assoc-ref (cdr b) ctx:set! #f))
    (define get (ast:lambda '()  (if b@ (b@ (car b)) ast:true)))
    (define set (ast:lambda '(v) (if b! (b! (car b) (ast:var 'v)) ast:true)))
    (if (not (or b@ b!)) renv
      (cons (ast:cons (ast:quote (car b)) (ast:cons get set)) renv)))
  (ast:vector (apply ast:list (foldr cons-caps '() env)) (ast:quote env)))

,(
;; Parsers for primitive syntax (no var dependencies)
(define parsers:primitive
  '((apply ((length=? 3 form) (ast:apply (loop (@ 1)) (loop (@ 2)))))
    (quote ((length=? 2 form) (ast:quote (@ 1))))
    (if    ((length=? 4 form) (ast:if (loop (@ 1)) (loop (@ 2)) (loop (@ 3)))))
    (set!  ((and (length=? 3 form) (name? (@ 1)))
            (parse:set! env (@ 1) (loop (@ 2)))))
    (reset ((length>=? 2 form) (ast:reset (parse:body* env (@. 1)))))
    (shift ((length>=? 3 form)
            (ast:shift (parse:lambda env (list (@ 1)) (@. 2)))))
    (lambda ((length>=? 3 form) (parse:lambda env (@ 1) (@. 2))))
    (letrec ((and (length>=? 3 form) (bpair*?! (@ 1)))
             (parse:letrec env (map car (@ 1)) (map cadr (@ 1)) (@. 2))))
    (let . (((and (length>=? 4 form) (name? (@ 1)) (bpair*?! (@ 2)))
             (define (ex-proc env) (parse:lambda env (map car (@ 2)) (@. 3)))
             (define (continue env) (parse:apply* env (@ 1) (map cadr (@ 2))))
             (parse:letrec env (list (@ 1)) (list ex-proc) (list continue)))
            ((and (length>=? 3 form) (bpair*?! (@ 1)))
             (parse:let env (map car (@ 1)) (map cadr (@ 1)) (@. 2)))))
    (let* ((and (length>=? 3 form) (bpair*?! (@ 1)))
           (let loop ((b* (@ 1)) (env env))
             (define (next env) (loop (cdr b*) env))
             (cond ((null? b*) (parse:body* env (@. 2)))
                   ((pair? b*) (parse:let env (list (caar b*))
                                          (list (cadar b*)) (list next)))))))
    (begin ((length>=? 2 form) (define rb* (reverse (map loop (@. 1))))
                               (ast:begin (reverse (cdr rb*)) (car rb*))))
    (cond ((and (list? form) (andmap (lambda (c) (length>=? 1 c)) (@. 1)))
           (define (ast:clause c $*)
             (if (null? (cdr c)) (parse:or2 env (car c) $*)
               (ast:if (loop (car c)) (parse:body* env (cdr c)) $*)))
           (foldr ast:clause ast:true (@. 1))))
    (and ((length>=? 1 form) (parse:and* env (@. 1))))
    (or  ((length>=? 1 form)
          (foldr (lambda (e r) (parse:or2 env e r)) ast:false (@. 1))))
    (when   ((length>=? 3 form)
             (ast:if (loop (@ 1)) (parse:body* env (@. 2)) ast:true)))
    (unless ((length>=? 3 form)
             (ast:if (loop (@ 1)) ast:true (parse:body* env (@. 2)))))))

;; Primitive language definition
(define code:syntax
  (map (lambda (name&clause*)
         (list 'cons (list 'quote (car name&clause*))
               (list 'lambda '(env form)
                     '(define (loop form) (parse env form))
                     '(define (@ i) (list-ref form i))
                     '(define (@. i) (list-tail form i))
                     (cons 'cond (append (cdr name&clause*)
                                         '((#t (error '"invalid syntax:"
                                                      form))))))))
       parsers:primitive))
(define code:prims
  '(map (lambda (po-desc)
          (define name (car po-desc))
          (cons name
                (lambda (env form)
                  (cond ((length=? (+ (length (cadr po-desc)) 1) form)
                         (ast:prim name (parse* env (list-tail form 1))))
                        (#t (error '"invalid primitive op:"
                                   po-desc form))))))
        primitive-op-descriptions))
(define code:ops (cons 'list* (append code:syntax (list code:prims))))
(define code:env:primitive
  (list 'env-extend*/syntax
        (list 'env-extend*/syntax 'env:empty 'ctx:op code:ops)
        'ctx:def '(list (cons 'begin parse:begin)
                        (cons 'define parse:define))))
(list 'begin (list 'define 'env:primitive code:env:primitive)
      '(define (lang:primitive form) (parse env:primitive form)))
)

;; Base language definition
(define primitive-op-procs
  (map (lambda (po-desc)
         (define (x i) (string-append 'x (number->string i)))
         (define p* (map x (range (length (cadr po-desc)))))
         (list (car po-desc) (list 'lambda p* (cons (car po-desc) p*))))
       primitive-op-descriptions))

(define derived-op-procs
  '((error (lambda args ('error args)))
    (not (lambda (b) (if b #f #t)))
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
    (list*  (lambda (x . xs) (if (null? xs) x (cons x (apply (list* xs))))))
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
    (caar (lambda (v) (car (car v))))
    (assoc (lambda (k xs) (cond ((null? xs) #f)
                                ((equal? k (caar xs)) (car xs))
                                (#t (assoc k (cdr xs))))))
    (remove-duplicates
      (lambda (xs)
        (define (ucons x acc) (if (member x acc) acc (cons x acc)))
        (reverse (foldl ucons '() xs))))))

(define code:cons*
  '((cons* (lambda (x xs) (if (null? xs) x
                            (cons x (cons* (car xs) (cdr xs))))))))
(define code:apply '((apply (lambda (f x . xs) (apply f (cons* x xs))))))
(define expander:base (lambda (env)
                        (define env:base (env-freeze env))
                        (define form (shift k k))
                        (parse env:base form)))
(define code (list 'let primitive-op-procs
                   (list 'letrec code:cons*
                         (list 'let code:apply (list 'letrec derived-op-procs
                                                     expander:base)))))
(define lang:base (reset (lang:primitive code)))
