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

(define (apply-curious program fn arg0 arg1* k)
  (let ((ctor (and (e-cons? arg0) (e-cons-c arg0))))
    (if ctor
      (let loop ((clause* (fn-curious-clause* fn)))
        (cond
          ((null? clause*) (format "invalid ctor: ~s ~s" arg0 fn))
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
       (apply-curious program fn (eval-expr (car arg*)) (cdr arg*) eval-expr))
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
                (apply-curious prog fn arg0 arg1* s-transient))
               ((e-var? arg0)
                (s-variants
                  (map (lambda (pc)
                         (define pat (p-clause-pattern pc))
                         (define ce (e-cons (e-cons-c pat)
                                            (map fresh-var (e-cons-ea* pat))))
                         (define cp (e-cons (e-cons-c pat)
                                            (map fresh-name (e-cons-ea* pat))))
                         (apply-curious
                           prog fn ce arg1*
                           (lambda (t) (s-variant (e-var-name arg0) cp t))))
                       (fn-curious-clause* fn))))
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
                       (else (error 'drive (format "impossible inner: ~s"
                                                   inner))))))))))))
      (else (error 'drive (format "invalid expr: ~s" expr)))))
  drive)

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
        (cond ((null? xs) '())
              ((eqv? (k x) (k (car xs))) (eqv? (v x) (v (car xs))))
              (else (check k v x (cdr xs)))))
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
  (define tree (simplify-tree
                 (fold-tree (build-tree (drive-machine prog)
                                        (subst e expr) size-max))))
  (define task (tree->task (program-ctor* prog) tree))
  `((tree: ,(print-tree depth tree))
    (program: ,(print-program (car task)))
    (expr: ,(print-expr (cadr task)))))


(define prog1
  `(((False 0) (True 0) (Z 0) (S 1))

    (define (add (Z) y) y)
    (define (add (S x) y) (S (add x y)))

    (define (mult (Z) y) (Z))
    (define (mult (S x) y) (add y (mult x y)))

    (define (sqr x) (mult x x))

    (define (even (Z)) (True))
    (define (even (S x)) (odd x))

    (define (odd (Z)) (False))
    (define (odd (S x)) (even x))

    (define (add2 (Z) y) y)
    (define (add2 (S x) y) (add2 x (S y)))
    ))
