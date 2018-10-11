(provide lang:base parse)

(require ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:reset ast:shift ast:prim primitive-op-descriptions)

;; Pattern matching
(define (length=? len xs)  (and (list? xs) (= (length xs) len)))
(define (length>=? len xs) (and (list? xs) (>= (length xs) len)))

;; Syntactic environments
(define ctx:var  'ref)
(define ctx:set! 'set!)
(define ctx:op   'syntax?)
(define ctx:def  'define)
(define env:empty                      '())
(define (env-ref env n)                (alist-ref env n '()))
(define (env-ref-prop env n k default) (alist-ref (env-ref env n) k default))
(define (env-extend*/var env p*)
  (define (bind n) (cons n (list (cons ctx:var  (lambda ()  (ast:var n)))
                                 (cons ctx:set! (lambda (v) (ast:set! n v))))))
  (append (map bind (filter (lambda (p?) p?) p*)) env))
(define (env-extend*/syntax env ctx b*)
  (define (bind b) (let ((n (car b)))
                     (cons n (cons (cons ctx (cdr b)) (env-ref env n)))))
  (append (map bind b*) (alist-remove* env (map car b*))))
(define (env-freeze env)
  (map (lambda (b) (cons (car b) (alist-remove* (cdr b) (list ctx:set!))))
       env))

;; Parameters
(define (param? p) (or (not p) (string? p)))
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
  (cond ((pair? form)
         (let ((p (car form)) (a* (cdr form)))
           (let ((op (and (string? p) (env-ref-prop env p ctx:op #f))))
             ;; TODO: (op env (cdr form))
             (if op (op env form) (parse:apply* env p a*)))))
        ((string? form) ((or (env-ref-prop env form ctx:var #f)
                             (error '"unbound variable:" form))))
        ((or (boolean? form) (number? form)) (ast:quote form))
        ((procedure? form)                   (form env))
        (#t                                  (error '"invalid syntax:" form))))
(define (parse* env form*) (map (lambda (f) (parse env f)) form*))

(define (parse:apply* env p a*) (ast:apply* (parse env p) (parse* env a*)))
(define (parse:lambda env ~ps body*)
  (let loop ((~p?* ~ps) (p* '()))
    (define (valid?! p)
      (unless (param? p)          (error '"invalid parameters:" ~ps))
      (when (and p (member p p*)) (error '"duplicate parameters:" ~ps)))
    (cond ((pair? ~p?*)       (define p (car ~p?*)) (valid?! p)
                              (loop (cdr ~p?*)  (if p (cons p p*) p*)))
          ((not (null? ~p?*)) (loop (list ~p?*) p*))
          (#t (let ((cenv (env-extend*/var (alist-remove* env p*) p*)))
                (ast:lambda ~ps (parse:body* cenv body*)))))))
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
(define (@define st n def-body)
  (vector (defst-env st) (cons (cons n def-body) (defst-rdef* st))))
;; TODO: $define-syntax ?
(define (parse:definition* st form*)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($define (and n (env-ref-prop (defst-env st) n ctx:def #f))))
             ;; TODO: ($define st (cdr form))
             (cond ($define ($define st form))
                   (#t (@define st #f form))))) st form*))
(define (parse:begin st form)
  (cond ((length>=? 1 form) (parse:definition* st (list-tail form 1)))
        (#t                 (error '"invalid begin:" form))))
(define (parse:define st form)
  (define (@ i) (list-ref form i)) (define (@. i) (list-tail form i))
  (cond ((and (length=? 3 form) (param? (@ 1))) (@define st (@ 1) (@ 2)))
        ((and (length>=? 3 form) (pair? (@ 1)) (string? (car (@ 1))))
         (define (edef env) (parse:lambda env (cdr (@ 1)) (@. 2)))
         (@define st (car (@ 1)) edef))
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

,(
;; Parsers for primitive syntax (no var dependencies)
(define parsers:primitive
  '((apply ((length=? 3 form) (ast:apply (loop (@ 1)) (loop (@ 2)))))
    (quote ((length=? 2 form) (ast:quote (@ 1))))
    (if    ((length=? 4 form) (ast:if (loop (@ 1)) (loop (@ 2)) (loop (@ 3)))))
    (set!  ((and (length=? 3 form) (string? (@ 1)))
            ((or (env-ref-prop env (@ 1) ctx:set! #f)
                 (error '"cannot set! variable:" (@ 1))) (loop (@ 2)))))
    (reset ((length>=? 2 form) (ast:reset (parse:body* env (@. 1)))))
    (shift ((length>=? 3 form)
            (ast:shift (parse:lambda env (list (@ 1)) (@. 2)))))
    (lambda ((length>=? 3 form) (parse:lambda env (@ 1) (@. 2))))
    (letrec ((and (length>=? 3 form) (bpair*?! (@ 1)))
             (parse:letrec env (map car (@ 1)) (map cadr (@ 1)) (@. 2))))
    (let . (((and (length>=? 4 form) (string? (@ 1)) (bpair*?! (@ 2)))
             (define (p env) (parse:lambda env (map car (@ 2)) (@. 3)))
             (ast:apply* (parse:letrec env (list (@ 1)) (list p) (list (@ 1)))
                         (parse* env (map cadr (@ 2)))))
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
