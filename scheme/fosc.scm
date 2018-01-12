;; See: https://themonadreader.files.wordpress.com/2014/04/super-final.pdf

;; P ::= ((C1 arity1) ... (Cn arityn)) d1 ... dn
;; d ::= (define (f v1 ... vn) e)
;;     | (define (g p1 v1 ... vn) e1)
;;       ...
;;       (define (g pm v1 ... vn) em)
;; e ::= v
;;     | (C e1 ... en)
;;     | (f e1 ... en)
;; p ::= (C v1 ... vn)

(define (tagged? t v) (and (pair? v) (eq? t (car v))))

(define (env pa*) `(env ,pa*))
(define env-empty (env '()))
(define (env-pa* e) (cadr e))
(define (env-apply param* arg*) (env (bind-param* param* arg*)))
(define (env-let e p a)
  (env (append (bind-param* (list p) (list a)) (env-pa* e))))
(define (env-pa e name)
  (let loop ((pa* (env-pa* e)))
    (and (pair? pa*)
         (if (eq? name (caar pa*)) (cadar pa*) (loop (cdr pa*))))))

(define (binding p a) (list p a))
(define (bind-param* param* arg*)
  (cond
    ((and (null? param*) (null? arg*)) '())
    ((and (pair? param*) (pair? arg*))
     (cons (binding (car param*) (car arg*))
           (bind-param* (cdr param*) (cdr arg*))))
    (else (error 'bind-param* (format "invalid binding: ~s ~s" param* arg*)))))

(define (program ctor* def*) `(program ,ctor* ,def*))
(define (program? v) (tagged? 'program v))
(define (program-ctor* p) (cadr p))
(define (program-def* p) (caddr p))
(define (program-ctor p name)
  (let loop ((ctor* (program-ctor* p)))
    (and (pair? ctor*)
         (if (eq? name (caar ctor*)) (car ctor*) (loop (cdr ctor*))))))
(define (program-fn p name)
  (let loop ((fn* (program-def* p)))
    (and (pair? fn*)
         (if (eq? name (fn-name (car fn*))) (car fn*) (loop (cdr fn*))))))

(define (fn-indifferent name param* body) `(fn-i ,name ,param* ,body))
(define (fn-indifferent? v) (tagged? 'fn-i v))
(define (fn-indifferent-name fn) (cadr fn))
(define (fn-indifferent-param* fn) (caddr fn))
(define (fn-indifferent-body fn) (cadddr fn))
(define (fn-indifferent-arity fn) (length (fn-indifferent-param* fn)))

(define (fn-curious name clause*) `(fn-c ,name ,clause*))
(define (fn-curious? v) (tagged? 'fn-c v))
(define (fn-curious-name fn) (cadr fn))
(define (fn-curious-clause* fn) (caddr fn))
(define (fn-curious-arity fn)
  (+ 1 (length (p-clause-param* (car (fn-curious-clause* fn))))))
(define (fn-curious-extend fn clause*)
  (and (or (= (fn-curious-arity fn)
              (+ 1 (length (p-clause-param* (car clause*)))))
           (error 'fn-curious-extend
                  (format "mismatching arity: ~s ~s" fn clause*)))
       (fn-curious (fn-curious-name fn)
                   (append clause* (fn-curious-clause* fn)))))

(define (fn-name fn)
  (or (and (fn-indifferent? fn) (fn-indifferent-name fn))
      (and (fn-curious? fn) (fn-curious-name fn))))
(define (fn-arity fn)
  (or (and (fn-indifferent? fn) (fn-indifferent-arity fn))
      (and (fn-curious? fn) (fn-curious-arity fn))))

(define (p-clause pattern param* body) `(p-clause ,pattern ,param* ,body))
(define (p-clause? v) (tagged? 'p-clause v))
(define (p-clause-pattern c) (cadr c))
(define (p-clause-param* c) (caddr c))
(define (p-clause-body c) (cadddr c))

(define (e-cons c ea*) `(e-cons ,c ,ea*))
(define (e-cons? v) (tagged? 'e-cons v))
(define (e-cons-c e) (cadr e))
(define (e-cons-ea* e) (caddr e))
(define (e-app f ea*) `(e-app ,f ,ea*))
(define (e-app? v) (tagged? 'e-app v))
(define (e-app-f e) (cadr e))
(define (e-app-ea* e) (caddr e))
(define (e-var name) `(e-var ,name))
(define (e-var? v) (tagged? 'e-var v))
(define (e-var-name e) (cadr e))
(define (e-let param arg body) `(e-let ,param ,arg ,body))
(define (e-let? e) (tagged? 'e-let e))
(define (e-let-param e) (cadr e))
(define (e-let-arg e) (caddr e))
(define (e-let-body e) (cadddr e))


(define (parse-program stx)
  (or (and (pair? stx)
           (let* ((ctor* (parse-ctor* (car stx)))
                  (p (program ctor* (parse-def* ctor* (cdr stx))))
                  (_ (validate-program p)))
             p))
      (error 'parse-program (format "invalid program: ~s" stx))))

(define (parse-ctor* stx)
  (or (null? stx)
      (and (pair? stx)
           (pair? (car stx))
           (symbol? (caar stx))
           (pair? (cdar stx))
           (number? (cadar stx))
           (null? (cddar stx))
           (parse-ctor* (cdr stx))
           (not (assoc (caar stx) (cdr stx)))
           stx)
      (error 'parse-ctor* (format "invalid constructor list: ~s" stx))))

(define (parse-def* ctor* stx)
  (define (parse-def stx)
    (if (and (tagged? 'define stx)
             (pair? (cdr stx))
             (pair? (cddr stx))
             (null? (cdddr stx))
             (pair? (cadr stx))
             (symbol? (caadr stx)))
      (if (and (pair? (cdadr stx)) (pair? (cadadr stx)))
        (parse-def-curious (caadr stx) (cadadr stx) (cddadr stx) (caddr stx))
        (parse-def-indifferent (caadr stx) (cdadr stx) (caddr stx)))
      (error 'parse-def (format "invalid definition: ~s" stx))))
  (define (parse-def-indifferent name param*-stx body-stx)
    (define param* (parse-name* param*-stx))
    (define body (parse-expr (program ctor* '())
                             (env-apply param* param*) body-stx))
    (fn-indifferent name param* body))
  (define (parse-def-curious name pat-stx param*-stx body-stx)
    (define pattern (parse-pattern ctor* pat-stx))
    (define param* (parse-name* param*-stx))
    (define pa* (append (e-cons-ea* pattern) param*))
    (define body (parse-expr (program ctor* '()) (env-apply pa* pa*) body-stx))
    (fn-curious name (list (p-clause pattern param* body))))
  (define (extend def* def)
    (if (and (fn-curious? def) (pair? def*) (fn-curious? (car def*))
             (eq? (fn-curious-name def) (fn-curious-name (car def*))))
      (cons (fn-curious-extend (car def*) (fn-curious-clause* def)) (cdr def*))
      (or (and (not (program-fn (program ctor* def*) (fn-name def)))
               (cons def def*))
          (error 'parse-def* (format "name defined multiple times: ~s ~s"
                                     (fn-name def) stx)))))
  (cond
    ((null? stx) '())
    ((pair? stx) (extend (parse-def* ctor* (cdr stx)) (parse-def (car stx))))
    (else (error 'parse-def* (format "invalid definition list: ~s" stx)))))

(define (parse-name* name*)
  (and (or (null? name*)
           (and (pair? name*) (symbol? (car name*)) (parse-name* (cdr name*))
                (not (memq (car name*) (cdr name*))))
           (error 'parse-name* (format "invalid name list: ~s" name*)))
       name*))
(define (parse-pattern ctor* stx)
  (or (and (pair? stx) (symbol? (car stx))
           (let* ((p (e-cons (car stx) (parse-name* (cdr stx))))
                  (ctor (assoc (car stx) ctor*))
                  (arity (and ctor (cadr ctor))))
             (and ctor (= arity (length (e-cons-ea* p))) p)))
      (error 'parse-pattern (format "invalid pattern: ~s" stx))))

(define (parse-expr p e stx)
  (cond
    ((symbol? stx)
     (if (env-pa e stx) (e-var stx)
       (error 'parse-expr (format "unbound variable: ~s ~s" e stx))))
    ((and (pair? stx) (eq? 'let (car stx)))
     (if (and (pair? (cdr stx)) (symbol? (cadr stx))
              (pair? (cddr stx)) (pair? (cdddr stx)) (null? (cddddr stx)))
       (e-let (cadr stx) (parse-expr p e (caddr stx))
              (parse-expr p (env-let e (cadr stx) (cadr stx)) (cadddr stx)))
       (error 'parse-expr (format "invalid let expr: ~s" stx))))
    ((and (pair? stx) (symbol? (car stx)))
     (let ((arg* (map (lambda (stx) (parse-expr p e stx)) (cdr stx)))
           (ctor (program-ctor p (car stx))))
       (if ctor
         (if (= (cadr ctor) (length arg*))
           (e-cons (car ctor) arg*)
           (error 'parse-expr (format "invalid constructor arity: ~s" stx)))
         (e-app (car stx) arg*))))
    (else (error 'parse-expr (format "invalid expression: ~s" stx)))))

(define (validate-program program)
  (define (validate-def fn)
    (if (fn-indifferent? fn)
      (validate-body (fn-indifferent-body fn))
      (andmap validate-body (map p-clause-body (fn-curious-clause* fn)))))
  (define (validate-body body)
    (cond
      ((e-app? body)
       (let ((fn (or (program-fn program (e-app-f body))
                     (error 'validate-program (format "undefined function: ~s"
                                                      (print-expr body))))))
         (and fn (or (= (fn-arity fn) (length (e-app-ea* body)))
                     (error 'validate-program
                            (format "invalid application arity: ~s ~s"
                                    (fn-arity fn) (print-expr body))))
              (andmap validate-body (e-app-ea* body)))))
      ((e-cons? body) (andmap validate-body (e-cons-ea* body)))
      ((e-let? body) (and (validate-body (e-let-arg body))
                          (validate-body (e-let-body body))))
      (else #t)))
  (andmap validate-def (program-def* program)))

(define (list-foldl f acc xs)
  (if (null? xs) acc (list-foldl f (f (car xs) acc) (cdr xs))))
(define (list-foldr f acc xs)
  (if (null? xs) acc (f (car xs) (list-foldr f acc (cdr xs)))))
(define (flatten xss) (list-foldr append '() xss))

(define (print-program program)
  (cons (program-ctor* program)
        (flatten (map print-def (program-def* program)))))
(define (print-def fn)
  (define name (fn-name fn))
  (define (print-fnc c)
    (define pat (p-clause-pattern c))
    `(define (,name (,(e-cons-c pat) . ,(e-cons-ea* pat))
                    . ,(p-clause-param* c))
       ,(print-expr (p-clause-body c))))
  (if (fn-indifferent? fn)
    `((define (,name . ,(fn-indifferent-param* fn))
        ,(print-expr (fn-indifferent-body fn))))
    (map print-fnc (fn-curious-clause* fn))))
(define (print-expr e)
  (define (print-fn fn) (if (symbol? fn) fn (fn-name fn)))
  (cond
    ((e-var? e) (e-var-name e))
    ((e-cons? e) (cons (e-cons-c e) (map print-expr (e-cons-ea* e))))
    ((e-app? e) (cons (print-fn (e-app-f e)) (map print-expr (e-app-ea* e))))
    ((e-let? e) `(let ,(e-let-param e) ,(print-expr (e-let-arg e))
                   ,(print-expr (e-let-body e))))
    (else #f)))

(define (sum xs) (list-foldl + 0 xs))
(define (free-vars expr)
  (define (free-vars* e*)
    (define (insert n ns) (if (memv n ns) ns (cons n ns)))
    (list-foldr (lambda (e ns) (list-foldr insert ns (free-vars e))) '() e*))
  (cond ((e-var? expr) (list (e-var-name expr)))
        ((e-cons? expr) (free-vars* (e-cons-ea* expr)))
        ((e-app? expr) (free-vars* (e-app-ea* expr)))
        ((e-let? expr)
         (remove (e-let-param expr)
                 (free-vars* (list (e-let-arg expr) (e-let-body expr)))))
        (else (error 'free-vars (format "invalid expr: ~s" expr)))))
(define (var-count name expr)
  (define (self expr) (var-count name expr))
  (cond ((e-var? expr) (if (eqv? name (e-var-name expr)) 1 0))
        ((e-cons? expr) (sum (map self (e-cons-ea* expr))))
        ((e-app? expr) (sum (map self (e-app-ea* expr))))
        ((e-let? expr)
         (+ (self (e-let-arg expr)) (if (eqv? name (e-let-param expr)) 0
                                      (self (e-let-body expr)))))
        (else (error 'var-count (format "invalid expr: ~s" expr)))))

(define (subst e expr)
  (define (subst* e es) (map (lambda (ea) (subst e ea)) es))
  (cond
    ((e-var? expr) (or (env-pa e (e-var-name expr)) expr))
    ((e-cons? expr) (e-cons (e-cons-c expr) (subst* e (e-cons-ea* expr))))
    ((e-app? expr) (e-app (e-app-f expr) (subst* e (e-app-ea* expr))))
    ((e-let? expr) (subst-let e expr))
    (else (error 'subst (format "invalid expr: ~s" expr)))))
(define (subst-let e expr)
  (subst (env-let e (e-let-param expr) (e-let-arg expr)) (e-let-body expr)))

(define (apply-curious program fn arg0 arg1* k not-hypothetical?)
  (let ((ctor (and (e-cons? arg0) (e-cons-c arg0))))
    (if ctor
      (let loop ((clause* (fn-curious-clause* fn)))
        (cond
          ((null? clause*)
           (and not-hypothetical?
                (error 'apply-curious (format "invalid ctor: ~s ~s" arg0 fn))))
          ((eqv? (e-cons-c (p-clause-pattern (car clause*))) ctor)
           (let* ((pp* (e-cons-ea* (p-clause-pattern (car clause*))))
                  (param* (append pp* (p-clause-param* (car clause*))))
                  (a* (append (e-cons-ea* arg0) arg1*)))
             (k (subst (env-apply param* a*) (p-clause-body (car clause*))))))
          (else (loop (cdr clause*)))))
      (e-app (fn-curious-name fn) (cons arg0 arg1*)))))

(define (eval/program program expr)
  (define (apply-fn fn arg*)
    (cond
      ((fn-indifferent? fn)
       (let ((e (env-apply (fn-indifferent-param* fn) arg*)))
         (eval-expr (subst e (fn-indifferent-body fn)))))
      ((fn-curious? fn)
       (apply-curious
         program fn (eval-expr (car arg*)) (cdr arg*) eval-expr #t))
      (else (error 'apply-fn (format "invalid fn: ~s" fn)))))
  (define (eval-expr expr)
    (cond
      ((or (e-var? expr) (e-cons? expr)) expr)
      ((e-app? expr) (apply-fn (program-fn program (e-app-f expr))
                               (e-app-ea* expr)))
      (else (error 'eval-expr (format "invalid expr: ~s" expr)))))
  (define (eval*-expr expr0)
    (define (ee* es) (map eval*-expr es))
    (define expr (eval-expr expr0))
    (cond
      ((e-var? expr) expr)
      ((e-cons? expr) (e-cons (e-cons-c expr) (ee* (e-cons-ea* expr))))
      ((e-app? expr) (e-app (e-app-f expr) (ee* (e-app-ea* expr))))
      (else (error 'eval*-expr (format "invalid expr: ~s" expr)))))
  (eval*-expr (subst env-empty expr)))

(define (parse-eval-print pstx estx free)
  (define prog (parse-program pstx))
  (define expr (parse-expr prog (env-apply free (map e-var free)) estx))
  (print-expr (eval/program prog expr)))


(define s-stop '(stop))
(define (s-stop? v) (tagged? 'stop v))
(define (s-transient e) `(transient ,e))
(define (s-transient? v) (tagged? 'transient v))
(define (s-transient-e v) (cadr v))
(define (s-decompose parts) `(decompose ,parts))
(define (s-decompose? v) (tagged? 'decompose v))
(define (s-decompose-parts v) (cadr v))
(define (s-variants choices) `(variants ,choices))
(define (s-variants? v) (tagged? 'variants v))
(define (s-variants-choices v) (cadr v))
(define (s-variant var pat body) `(,var ,pat ,body))
(define (s-variant-var v) (car v))
(define (s-variant-pat v) (cadr v))
(define (s-variant-body v) (caddr v))
(define (s-fold label renaming) `(fold ,label ,renaming))
(define (s-fold? v) (tagged? 'fold v))
(define (s-fold-label v) (cadr v))
(define (s-fold-renaming v) (caddr v))

(define (fresh-name name) (gensym (symbol->string name)))
(define (fresh-var name) (e-var (fresh-name name)))

(define (drive-machine prog)
  (define (drive expr)
    (cond
      ((e-var? expr) s-stop)
      ((and (e-cons? expr) (null? (e-cons-ea* expr))) s-stop)
      ((e-cons? expr) (s-decompose (e-cons-ea* expr)))
      ((e-let? expr) (s-decompose (list (e-let-arg expr) (e-let-body expr))))
      ((e-app? expr)
       (let ((fn (program-fn prog (e-app-f expr))))
         (if (fn-indifferent? fn)
           (s-transient
             (subst (env-apply (fn-indifferent-param* fn)
                               (e-app-ea* expr)) (fn-indifferent-body fn)))
           (let ((arg0 (car (e-app-ea* expr))) (arg1* (cdr (e-app-ea* expr))))
             (cond
               ((e-cons? arg0)
                (or (apply-curious prog fn arg0 arg1* s-transient #f) s-stop))
               ((e-var? arg0)
                (let ((alts (filter (lambda (x) x)
                                    (map (lambda (pc)
                                           (define pat
                                             (p-clause-pattern pc))
                                           (define vs
                                             (map fresh-var (e-cons-ea* pat)))
                                           (define ce
                                             (e-cons (e-cons-c pat) vs))
                                           (define p
                                             (e-cons (e-cons-c pat)
                                                     (map e-var-name vs)))
                                           (apply-curious
                                             prog fn ce arg1*
                                             (lambda (t)
                                               (s-variant
                                                 (e-var-name arg0) p t))
                                             #f))
                                         (fn-curious-clause* fn)))))
                  (if (null? alts) s-stop (s-variants alts))))
               (else
                 (lambda ()
                   (let retry ((inner (drive arg0)))
                     (define (ctx t) (e-app (e-app-f expr) (cons t arg1*)))
                     (cond
                       ((procedure? inner) (retry (inner)))
                       ((s-transient? inner)
                        (s-transient (ctx (s-transient-e inner))))
                       ((s-variants? inner)
                        (s-variants
                          (map (lambda (v)
                                 (s-variant (s-variant-var v)
                                            (s-variant-pat v)
                                            (ctx (s-variant-body v))))
                               (s-variants-choices inner))))
                       ((s-stop? inner) s-stop)
                       (else (error 'drive (format "impossible inner: ~s"
                                                   inner))))))))))))
      (else (error 'drive (format "invalid expr: ~s" expr)))))
  drive)

(define (propagate-vps step)
  (cond
    ((procedure? step) (lambda () (propagate-vps (step))))
    ((s-variants? step)
     (s-variants
       (map (lambda (c)
              (define cv (s-variant-var c))
              (define cp (s-variant-pat c))
              (define cd (e-cons (e-cons-c cp) (map e-var (e-cons-ea* cp))))
              (s-variant cv cp (subst (env (list (binding cv cd)))
                                      (s-variant-body c))))
            (s-variants-choices step))))
    (else step)))

(define (step-map f step)
  (let retry ((step step))
    (cond ((procedure? step) (retry (step)))
          ((s-transient? step) (s-transient (f (s-transient-e step))))
          ((s-decompose? step) (s-decompose (map f (s-decompose-parts step))))
          ((s-variants? step)
           (s-variants (map (lambda (v) (s-variant (s-variant-var v)
                                                   (s-variant-pat v)
                                                   (f (s-variant-body v))))
                            (s-variants-choices step))))
          ((or (s-stop? step) (s-fold? step)) step)
          (else (error 'step-map (format "invalid step: ~s" step))))))

(define (size-expr e)
  (cond ((e-var? e) 1)
        ((e-cons? e) (apply + 1 (map size-expr (e-cons-ea* e))))
        ((e-app? e) (apply + 1 (map size-expr (e-app-ea* e))))
        ((e-let? e) (+ 1 (size-expr (e-let-arg e)) (size-expr (e-let-body e))))
        (else (error 'size-expr (format "cannot size expr: ~s" e)))))

(define (can-generalize expr)
  (and (e-app? expr) (not (andmap e-var? (e-app-ea* expr)))))

(define (generalize expr)
  (when (not (e-app? expr)) (error 'generalize (format "non-app: ~s" expr)))
  (let* ((var (fresh-var 'g)) (fn (e-app-f expr)) (ea* (e-app-ea* expr))
         (size-max (apply max (map size-expr ea*))))
    (if (null? ea*) expr
      (let loop ((prefix '()) (suffix ea*))
        (if (= size-max (size-expr (car suffix)))
          (e-let (e-var-name var) (car suffix)
            (e-app fn (append prefix (cons var (cdr suffix)))))
          (loop (cons (car suffix) prefix) (cdr suffix)))))))

(define (node label expr step) (list label expr step))
(define (node-label n) (car n))
(define (node-expr n) (cadr n))
(define (node-step n) (caddr n))

(define (build-tree drive expr size-max)
  (let bt ((expr expr))
    (if (and size-max (can-generalize expr) (< size-max (size-expr expr)))
      (bt (generalize expr))
      (node (gensym "l") expr (lambda () (step-map bt (drive expr)))))))

(define (print-tree depth tree)
  (define (pt tree)
    (print-tree (and depth (if (= 0 depth) 0 (- depth 1))) tree))
  (define step (node-step tree))
  (node (node-label tree) (print-expr (node-expr tree))
        (if (and (procedure? step) (eqv? 0 depth)) step (step-map pt step))))

(define (renaming e1 e2)
  (define (o2o* e1s e2s)
    (define (cons-bi-unique a as)
      (define (check k v x xs)
        (and xs (cond ((null? xs) '())
                      ((eqv? (k x) (k (car xs))) (eqv? (v x) (v (car xs))))
                      (else (check k v x (cdr xs))))))
      (let ((c1 (check car cdr a as)))
        (and c1 (or (and (eq? #t c1) as)
                    (let ((c2 (check cdr car a as))) (and c2 (cons a as)))))))
    (define (append-bi-unique as bs) (list-foldr cons-bi-unique bs as))
    (if (null? e1s) '()
      (let ((result (renaming (car e1s) (car e2s))))
        (and result (let ((result* (o2o* (cdr e1s) (cdr e2s))))
                      (and result* (append-bi-unique result result*)))))))
  (define (both? p?) (and (p? e1) (p? e2)))
  (define (proj=? proj) (eqv? (proj e1) (proj e2)))
  (define (lproj=? proj) (eqv? (length (proj e1)) (length (proj e2))))
  (cond ((and (both? e-app?) (proj=? e-app-f) (lproj=? e-app-ea*))
         (o2o* (e-app-ea* e1) (e-app-ea* e2)))
        ((and (both? e-cons?) (proj=? e-cons-c) (lproj=? e-cons-ea*))
         (o2o* (e-cons-ea* e1) (e-cons-ea* e2)))
        ((both? e-let?)
         (renaming (subst-let env-empty e1) (subst-let env-empty e2)))
        ((both? e-var?) (list (cons (e-var-name e1) (e-var-name e2))))
        (else #f)))

(define (fold-tree tree)
  (let loop ((tree tree) (seen '()))
    (node
      (node-label tree)
      (node-expr tree)
      (or (and (e-app? (node-expr tree))
               (let tie-back ((seen seen))
                 (and (pair? seen)
                      (let ((rn (renaming (node-expr (car seen))
                                          (node-expr tree))))
                        (or (and rn (s-fold (node-label (car seen)) rn))
                            (tie-back (cdr seen)))))))
          (step-map (lambda (t) (loop t (cons tree seen)))
                    (node-step tree))))))

(define (simplify-tree tree)
  (define (recursive? step)
    (cond ((s-transient? step) (recursive? (node-step (s-transient-e step))))
          ((s-decompose? step)
           (ormap recursive? (map node-step (s-decompose-parts step))))
          ((s-variants? step)
           (ormap recursive? (map node-step (map s-variant-body
                                                 (s-variants-choices step)))))
          ((s-fold? step) (eqv? (node-label tree) (s-fold-label step)))
          (else #f)))
  (define step (node-step tree))
  (if (and (s-transient? step) (not (recursive? step)))
    (simplify-tree (s-transient-e step))
    (node (node-label tree) (node-expr tree) (step-map simplify-tree step))))

(define (tree->task ctors tree)
  (define (tt* t* apps)
    (list-foldr (lambda (t edefs)
                  (let ((result (tt t apps)))
                    (list (cons (car result) (car edefs))
                          (append (cadr result) (cadr edefs)))))
                '(() ()) t*))
  (define (tt tree apps)
    (define label (node-label tree))
    (define expr (node-expr tree))
    (define step (node-step tree))
    (cond
      ((s-stop? (node-step tree)) (list (node-expr tree) '()))
      ((s-decompose? step)
       (let* ((ps-defs (tt* (s-decompose-parts step) apps))
              (parts (car ps-defs)) (defs (cadr ps-defs)))
         (list (if (e-cons? expr)
                 (e-cons (e-cons-c expr) parts)
                 (e-let (e-let-param expr) (car parts) (cadr parts))) defs)))
      ((s-transient? step)
       (let* ((vs (free-vars expr))
              (fname (gensym "f"))
              (app (e-app fname (map e-var vs)))
              (tbody (tt (s-transient-e step) (cons (cons label app) apps)))
              (body (car tbody))
              (defs (cons (fn-indifferent fname vs body) (cadr tbody))))
         (list app defs)))
      ((s-variants? step)
       (let* ((choices (s-variants-choices step))
              (cbodies (map s-variant-body choices))
              (pats (map s-variant-pat choices))
              (vs (free-vars expr))
              (pv (car vs))
              (multi? (and (< 1 (var-count pv expr))
                           (memv pv (flatten (map free-vars
                                                  (map node-expr cbodies))))))
              (param* (if multi? vs (cdr vs)))
              (vs (if multi? (cons pv vs) vs))
              (fname (gensym "g"))
              (app (e-app fname (map e-var vs)))
              (tbodies (tt* cbodies (cons (cons label app) apps)))
              (bodies (car tbodies))
              (defs (cons (fn-curious
                            fname (map (lambda (p b) (p-clause p param* b))
                                       pats bodies)) (cadr tbodies))))
         (list app defs)))
      ((s-fold? step)
       (list (subst (env (map (lambda (kv) (binding (car kv) (e-var (cdr kv))))
                              (s-fold-renaming step)))
                    (cdr (assoc (s-fold-label step) apps))) '()))
      (else (error 'tree->task (format "invalid step: ~s" step)))))
  (define result (tt tree '()))
  (list (program ctors (cadr result)) (car result)))


(define (parse-transform-print pstx estx free size-max depth)
  (define prog (parse-program pstx))
  (define e (env-apply free (map e-var free)))
  (define expr (parse-expr prog e estx))
  (define drive (lambda (e) (propagate-vps ((drive-machine prog) e))))
  (define tree (simplify-tree
                 (fold-tree (build-tree drive (subst e expr) size-max))))
  (define task (tree->task (program-ctor* prog) tree))
  `((tree: ,(print-tree depth tree))
    (program: ,(print-program (car task)))
    (expr: ,(print-expr (cadr task)))))


(define prog1
  '(((False 0) (True 0) (Z 0) (S 1)
     (quote 1) (+ 2) (* 2) (Even? 1) (Odd? 1) (lambda 1) (var 1) (app 2)
     (Nil 0) (Cons 2))

    (define (append (Nil) y) y)
    (define (append (Cons a d) y) (Cons a (append d y)))

    (define (mirror (Nil)) (Nil))
    (define (mirror (Cons a d)) (Cons (mirror d) (mirror a)))

    (define (add (Z) y) y)
    (define (add (S x) y) (S (add x y)))

    (define (mult (Z) y) (Z))
    (define (mult (S x) y) (add y (mult x y)))

    (define (sqr x) (mult x x))

    (define (even? (Z)) (True))
    (define (even? (S x)) (odd? x))

    (define (odd? (Z)) (False))
    (define (odd? (S x)) (even? x))

    (define (add2 (Z) y) y)
    (define (add2 (S x) y) (add2 x (S y)))

    (define (add3 (Z) y) y)
    (define (add3 (S x) y) (S (add3 y x)))

    (define (mult2 (Z) y) (Z))
    (define (mult2 (S x) y) (add3 (mult2 x y) y))

    (define (double (Z)) (Z))
    (define (double (S x)) (S (S (double x))))

    (define (eval (quote datum)) datum)
    (define (eval (+ a b)) (add3 (eval a) (eval b)))
    (define (eval (* a b)) (mult2 (eval a) (eval b)))
    (define (eval (Even? n)) (even? (eval n)))
    (define (eval (Odd? n)) (odd? (eval n)))
    (define (eval (lambda body)) (lambda body))
    (define (eval (app proc arg)) (apply (eval proc) (eval arg)))

    (define (apply (lambda body) arg) (eval (propagate body arg)))

    (define (propagate (quote datum) val) (quote datum))
    (define (propagate (+ a b) val) (+ (propagate a val) (propagate b val)))
    (define (propagate (* a b) val) (* (propagate a val) (propagate b val)))
    (define (propagate (Even? x) val) (Even? (propagate x val)))
    (define (propagate (Odd? x) val) (Odd? (propagate x val)))
    (define (propagate (lambda body) val) (lambda (propagate body (lift val))))
    (define (propagate (app proc arg) val)
      (app (propagate proc val) (propagate arg val)))
    (define (propagate (var idx) val) (substitute idx val))

    (define (lift (True)) (True))
    (define (lift (False)) (False))
    (define (lift (Z)) (Z))
    (define (lift (S x)) (S x))
    (define (lift (+ a b)) (+ (lift a) (lift b)))
    (define (lift (* a b)) (* (lift a) (lift b)))
    (define (lift (Even? x)) (Even? (lift x)))
    (define (lift (Odd? x)) (Odd? (lift x)))
    (define (lift (quote val)) (quote (lift val)))
    (define (lift (lambda body)) (lambda (lift body)))
    (define (lift (app proc arg)) (app (lift proc) (lift arg)))
    (define (lift (var idx)) (var (S idx)))

    (define (substitute (Z) val) (literal val))
    (define (substitute (S x) val) (var x))

    (define (literal (lambda body)) (lambda body))
    (define (literal (True)) '(True))
    (define (literal (False)) '(False))
    (define (literal (Z)) '(Z))
    (define (literal (S x)) '(S x))
    ))

;; prog1 examples
(define double-t
  (parse-transform-print
    prog1
    ;'(double X)
    ;'(add3 X X)
    '(mult2 (S (S (Z))) X)
    '(X)
    10
    1))

(define even-double-t
  (parse-transform-print
    prog1
    ;'(even? (double X))
    ;'(even? (add3 X X))
    '(even? (mult2 (S (S (Z))) X))
    '(X)
    40
    1))

(define add-assoc-t1
  (parse-transform-print
    prog1
    '(add (add W X) Y)
    '(W X Y)
    40
    1))

(define add-assoc-t2
  (parse-transform-print
    prog1
    '(add W (add X Y))
    '(W X Y)
    40
    1))

(define append-assoc-t1
  (parse-transform-print
    prog1
    '(append (append W X) Y)
    '(W X Y)
    40
    1))

(define append-assoc-t2
  (parse-transform-print
    prog1
    '(append W (append X Y))
    '(W X Y)
    40
    1))

(define mirror-mirror-identity
  (parse-transform-print
    prog1
    '(mirror (mirror X))
    '(X)
    40
    1))

(define futamura1-1
  (parse-transform-print
    prog1
    ;'(eval (+ 'X 'X))
    ;'(eval (Even? (+ 'X 'X)))
    ;'(eval (Even? (* '(S (S (Z))) 'X)))
    ;'(eval (Even? (* '(S (S (Z))) (+ '(S (Z)) 'X))))
    ;'(eval (Even? (+ '(S (Z)) (* '(S (S (Z))) (+ '(S (Z)) 'X)))))
    '(eval (Odd? (+ '(S (Z)) (* '(S (S (Z))) (+ '(S (Z)) 'X)))))
    '(X)
    40
    1))

(define futamura1-2
  (parse-transform-print
    prog1
    '(eval (app (lambda (+ (var (Z)) (var (Z)))) 'X))
    '(X)
    80 1))

(define futamura1-3
  (parse-transform-print
    prog1
    '(eval (app (app (lambda (var (Z)))
                     (lambda (+ (var (Z)) (var (Z)))))
                'X))
    '(X)
    80 1))

(define futamura1-4
  (parse-transform-print
    prog1
    '(eval (app (app (lambda
                       (lambda (app (app (var (Z)) (var (S (Z))))
                                    (var (S (Z))))))
                     (lambda
                       (lambda (+ (var (Z)) (var (S (Z)))))))
                'X))
    '(X)
    120 1))

(define futamura1-5
  (parse-transform-print
    prog1
    '(eval (app (app (lambda
                       (lambda (app (app (var (Z)) (var (S (Z))))
                                    (var (S (Z))))))
                     (lambda
                       (lambda (+ (var (Z)) (var (S (Z)))))))
                'X))
    '(X)
    120 1))

(define futamura1-6
  (parse-transform-print
    prog1
    '(eval (app (app (lambda
                       (lambda
                         (Even? (app (app (var (Z)) (var (S (Z))))
                                     (var (S (Z)))))))
                     (lambda
                       (lambda (+ (var (Z)) (var (S (Z)))))))
                'X))
    '(X)
    120 1))

(define futamura1-7
  (parse-transform-print
    prog1
    '(eval (Even? (app (app (lambda
                              (lambda
                                (app (app (var (Z)) (var (S (Z))))
                                     (var (S (Z))))))
                            (lambda
                              (lambda (+ (var (Z)) (var (S (Z)))))))
                       'X)))
    '(X)
    120 1))

(define kmp
  '(((False 0) (True 0) (Nil 0) (Cons 2) (A 0) (B 0))
    (define (if (True) t f) t)
    (define (if (False) t f) f)
    (define (A? (A)) (True))
    (define (A? (B)) (False))
    (define (B? (A)) (False))
    (define (B? (B)) (True))
    (define (eq (A) y) (A? y))
    (define (eq (B) y) (B? y))

    (define (match p s) (m p s p s))

    (define (m (Nil) ss op os) (True))
    (define (m (Cons p pp) ss op os) (x ss p pp op os))

    (define (x (Nil) p pp op os) (False))
    (define (x (Cons s ss) p pp op os) (if (eq p s) (m pp ss op os) (n os op)))

    (define (n (Nil) op) (False))
    (define (n (Cons s ss) op) (m op ss op ss))))

;; kmp examples
(define kmp1
  (parse-eval-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (A) (Cons (A) (Cons (B) (Nil)))))
    '()))

(define kmp2
  (parse-eval-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (B) (Cons (A) (Cons (A) (Cons (B) (Cons (A) (Nil)))))))
    '()))

(define kmp3
  (parse-eval-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (A) (Cons (B) (Cons (A) (Cons (B) (Cons (A) (Nil)))))))
    '()))

(define kmpx
  (parse-eval-print
    kmp
    '(match (Cons (A) (Cons (A) (Cons (B) (Nil)))) X)
    '(X)))

(define kmpt1
  (parse-transform-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (A) (Cons (A) (Cons (B) (Nil)))))
    '()
    80 20))

(define kmpt2
  (parse-transform-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (B) (Cons (A) (Cons (A) (Cons (B) (Cons (A) (Nil)))))))
    '()
    80 20))

(define kmpt3
  (parse-transform-print
    kmp
    '(match
       (Cons (A) (Cons (A) (Cons (B) (Nil))))
       (Cons (A) (Cons (B) (Cons (A) (Cons (B) (Cons (A) (Nil)))))))
    '()
    80 20))

(define kmptx
  (parse-transform-print
    kmp
    '(match (Cons (A) (Cons (A) (Cons (B) (Nil)))) X)
    '(X)
    40 20))

;; unsimplified transformations
(define kmpv1
  '(((False 0) (True 0) (Nil 0) (Cons 2) (A 0) (B 0))
    (define (f3144) (f3145))
    (define (f3145) (f3146))
    (define (f3146) (let g3126 (f3150) (f3147 g3126)))
    (define (f3150) (f3151))
    (define (f3151) (f3152))
    (define (f3152) (f3153))
    (define (f3153) (f3154))
    (define (f3154) (f3155))
    (define (f3155) (f3156))
    (define (f3156) (f3157))
    (define (f3157) (f3158))
    (define (f3158) (f3159))
    (define (f3159) (f3160))
    (define (f3160) (True))
    (define (f3147 g3126) (f3148 g3126))
    (define (f3148 g3126) (f3149 g3126))
    (define (f3149 g3126) g3126)))
  ;(expr: (f3144))

(define kmpv2
  '(((False 0) (True 0) (Nil 0) (Cons 2) (A 0) (B 0))
    (define (f3240) (f3241))
    (define (f3241) (f3242))
    (define (f3242) (let g3164 (f3263) (f3243 g3164)))
    (define (f3263) (f3264))
    (define (f3264) (let g3169 (f3268) (f3265 g3169)))
    (define (f3268) (f3269))
    (define (f3269) (let g3174 (f3290) (f3270 g3174)))
    (define (f3290) (True))
    (define (f3270 g3174) (f3271 g3174))
    (define (f3271 g3174) (f3272 g3174))
    (define (f3272 g3174) (f3273))
    (define (f3273) (f3274))
    (define (f3274) (f3275))
    (define (f3275) (let g3184 (f3279) (f3276 g3184)))
    (define (f3279) (f3280))
    (define (f3280) (let g3189 (f3284) (f3281 g3189)))
    (define (f3284) (f3285))
    (define (f3285) (let g3194 (f3289) (f3286 g3194)))
    (define (f3289) (True))
    (define (f3286 g3194) (f3287 g3194))
    (define (f3287 g3194) (f3288 g3194))
    (define (f3288 g3194) g3194)
    (define (f3281 g3189) (f3282 g3189))
    (define (f3282 g3189) (f3283 g3189))
    (define (f3283 g3189) g3189)
    (define (f3276 g3184) (f3277 g3184))
    (define (f3277 g3184) (f3278 g3184))
    (define (f3278 g3184) g3184)
    (define (f3265 g3169) (f3266 g3169))
    (define (f3266 g3169) (f3267 g3169))
    (define (f3267 g3169) g3169)
    (define (f3243 g3164) (f3244 g3164))
    (define (f3244 g3164) (f3245 g3164))
    (define (f3245 g3164) (f3246))
    (define (f3246) (f3247))
    (define (f3247) (f3248))
    (define (f3248) (let g3216 (f3252) (f3249 g3216)))
    (define (f3252) (f3253))
    (define (f3253) (let g3221 (f3257) (f3254 g3221)))
    (define (f3257) (f3258))
    (define (f3258) (let g3226 (f3262) (f3259 g3226)))
    (define (f3262) (True))
    (define (f3259 g3226) (f3260 g3226))
    (define (f3260 g3226) (f3261 g3226))
    (define (f3261 g3226) g3226)
    (define (f3254 g3221) (f3255 g3221))
    (define (f3255 g3221) (f3256 g3221))
    (define (f3256 g3221) g3221)
    (define (f3249 g3216) (f3250 g3216))
    (define (f3250 g3216) (f3251 g3216))
    (define (f3251 g3216) g3216)))
  ;(expr: (f3240))

(define kmpv3
  '(((False 0) (True 0) (Nil 0) (Cons 2) (A 0) (B 0))
    (define (f3426) (f3427))
    (define (f3427) (f3428))
    (define (f3428) (let g3294 (f3432) (f3429 g3294)))
    (define (f3432) (f3433))
    (define (f3433) (let g3299 (f3479) (f3434 g3299)))
    (define (f3479) (f3480))
    (define (f3480) (let g3304 (f3526) (f3481 g3304)))
    (define (f3526) (True))
    (define (f3481 g3304) (f3482 g3304))
    (define (f3482 g3304) (f3483 g3304))
    (define (f3483 g3304) (f3484))
    (define (f3484) (f3485))
    (define (f3485) (f3486))
    (define (f3486) (let g3314 (f3515) (f3487 g3314)))
    (define (f3515) (f3516))
    (define (f3516) (let g3319 (f3520) (f3517 g3319)))
    (define (f3520) (f3521))
    (define (f3521) (let g3324 (f3525) (f3522 g3324)))
    (define (f3525) (True))
    (define (f3522 g3324) (f3523 g3324))
    (define (f3523 g3324) (f3524 g3324))
    (define (f3524 g3324) g3324)
    (define (f3517 g3319) (f3518 g3319))
    (define (f3518 g3319) (f3519 g3319))
    (define (f3519 g3319) g3319)
    (define (f3487 g3314) (f3488 g3314))
    (define (f3488 g3314) (f3489 g3314))
    (define (f3489 g3314) (f3490))
    (define (f3490) (f3491))
    (define (f3491) (f3492))
    (define (f3492) (let g3340 (f3496) (f3493 g3340)))
    (define (f3496) (f3497))
    (define (f3497) (f3498))
    (define (f3498) (f3499))
    (define (f3499) (f3500))
    (define (f3500) (f3501))
    (define (f3501) (f3502))
    (define (f3502) (f3503))
    (define (f3503) (f3504))
    (define (f3504) (f3505))
    (define (f3505) (f3506))
    (define (f3506) (f3507))
    (define (f3507) (f3508))
    (define (f3508) (f3509))
    (define (f3509) (f3510))
    (define (f3510) (f3511))
    (define (f3511) (f3512))
    (define (f3512) (f3513))
    (define (f3513) (f3514))
    (define (f3514) (False))
    (define (f3493 g3340) (f3494 g3340))
    (define (f3494 g3340) (f3495 g3340))
    (define (f3495 g3340) g3340)
    (define (f3434 g3299) (f3435 g3299))
    (define (f3435 g3299) (f3436 g3299))
    (define (f3436 g3299) (f3437))
    (define (f3437) (f3438))
    (define (f3438) (f3439))
    (define (f3439) (let g3371 (f3468) (f3440 g3371)))
    (define (f3468) (f3469))
    (define (f3469) (let g3376 (f3473) (f3470 g3376)))
    (define (f3473) (f3474))
    (define (f3474) (let g3381 (f3478) (f3475 g3381)))
    (define (f3478) (True))
    (define (f3475 g3381) (f3476 g3381))
    (define (f3476 g3381) (f3477 g3381))
    (define (f3477 g3381) g3381)
    (define (f3470 g3376) (f3471 g3376))
    (define (f3471 g3376) (f3472 g3376))
    (define (f3472 g3376) g3376)
    (define (f3440 g3371) (f3441 g3371))
    (define (f3441 g3371) (f3442 g3371))
    (define (f3442 g3371) (f3443))
    (define (f3443) (f3444))
    (define (f3444) (f3445))
    (define (f3445) (let g3397 (f3449) (f3446 g3397)))
    (define (f3449) (f3450))
    (define (f3450) (f3451))
    (define (f3451) (f3452))
    (define (f3452) (f3453))
    (define (f3453) (f3454))
    (define (f3454) (f3455))
    (define (f3455) (f3456))
    (define (f3456) (f3457))
    (define (f3457) (f3458))
    (define (f3458) (f3459))
    (define (f3459) (f3460))
    (define (f3460) (f3461))
    (define (f3461) (f3462))
    (define (f3462) (f3463))
    (define (f3463) (f3464))
    (define (f3464) (f3465))
    (define (f3465) (f3466))
    (define (f3466) (f3467))
    (define (f3467) (False))
    (define (f3446 g3397) (f3447 g3397))
    (define (f3447 g3397) (f3448 g3397))
    (define (f3448 g3397) g3397)
    (define (f3429 g3294) (f3430 g3294))
    (define (f3430 g3294) (f3431 g3294))
    (define (f3431 g3294) g3294)))
;  (expr: (f3426))
