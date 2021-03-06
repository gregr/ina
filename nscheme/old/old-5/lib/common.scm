((provide length=? length>=? ctx:var ctx:set! ctx:op ctx:def
          env:empty env-ref env-get-prop
          env-remove* env-add* env-extend* env-update* name->string
          param?! bpair*?! ncons param-map param-names param-bind
          defstate:empty defstate-env defstate-names defstate-actions
          defstate-env-set defstate-names-add defstate-actions-add))

;; Pattern matching
(define (length=? len xs)  (and (list? xs) (= (length xs) len)))
(define (length>=? len xs) (and (list? xs) (>= (length xs) len)))
(define (name->string n) (if (mvector? n) (mvector-ref n 0) n))
(define (name? p) (and (or (not (mvector? p)) (= (mvector-length p) 1))
                       (string? (name->string p))))
(define (param?! param) (unless (andmap name? (param-names param))
                          (error '"invalid parameters:" param)))
(define (bpair*?! b*)
  (define (? b) (and (length=? 2 b) (param?! (car b))))
  (unless (and (list? b*) (andmap ? b*)) (error '"invalid binding list:" b*)))

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
    (cond ((pair? p)   (loop (car p) (loop (cdr p) ns)))
          ((vector? p) (loop (vector->list p) ns))
          ((null? p)   ns)
          ((not p)     ns)
          (#t          (ncons p ns)))))
(define (param-bind param arg)
  (let loop ((p param) (a arg) (bs '()))
    (cond ((and (pair? p) (pair? a))
           (loop (car p) (car a) (loop (cdr p) (cdr a) bs)))
          ((and (vector? p) (vector? a))
           (loop (vector->list p) (vector->list a) bs))
          ((and (null? p) (null? a))                  bs)
          ((not p)                                    bs)
          ((not (or (pair? p) (vector? p) (null? p))) (cons (cons p a) bs))
          (#t (error '"parameter/argument mismatch:"
                     (param-map name->string param) arg
                     (param-map name->string p) a)))))

;; Syntactic environments
(define ctx:var  'ref)
(define ctx:set! 'set!)
(define ctx:op   'syntax?)
(define ctx:def  'define)
(define env:empty                      '())
(define (env-ref env n)                (alist-get env n '()))
(define (env-get-prop env n k default) (alist-get (env-ref env n) k default))
(define (env-remove* env n*)           (alist-remove* env n*))
(define (env-add* env b*)              (append b* env))
(define (env-extend* env b*) (env-add* (env-remove* env (map car b*)) b*))
(define (env-update* env b*)
  (define (update b) (cons (car b) (append (cdr b) (env-ref env (car b)))))
  (env-extend* env (map update b*)))

;; Definition contexts
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
