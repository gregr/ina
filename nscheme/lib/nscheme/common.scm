(provide length=? length>=? ctx:var ctx:set! ctx:op ctx:def
         env:empty env-ref env-ref-prop
         param? bpair*?! ncons param-names param-bind
         defstate:empty defstate-env defstate-names defstate-actions
         defstate-env-set defstate-names-add defstate-actions-add)

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

;; Formal parameters
;; TODO: get rid of param?
(define (param? p) (or (not p) (string? p)))
(define (bpair*?! b*)
  ;; TODO: replace param? with param-names.
  (define (? b) (and (length=? 2 b) (param? (car b))))
  (unless (and (list? b*) (andmap ? b*)) (error '"invalid binding list:" b*)))
(define (ncons name names)
  (when (member name names) (error '"duplicate name:" name names))
  (cons name names))
(define (param-names param)
  (let loop ((p param) (ns '()))
    (cond ((pair? p)   (loop (cdr p) (loop (car p) ns)))
          ((vector? p) (loop (vector->list p) ns))
          ((string? p) (ncons p ns))
          ((null? p)   ns)
          ((not p)     ns)
          (else (error '"invalid parameter:" p param)))))
(define (param-bind param arg)
  (let loop ((p param) (a arg))
    (cond ((and (pair? p) (pair? a)) (append (loop (car p) (car a))
                                             (loop (cdr p) (cdr a))))
          ((and (vector? p) (vector? a))
           (loop (vector->list p) (vector->list a)))
          ((string? p)               (list (cons p a)))
          ((and (null? p) (null? a)) '())
          ((not p)                   '())
          (else (error '"parameter/argument mismatch:" param arg p a)))))

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
