(define (id v) v)
(define (id* . v) v)
(define (const k) (lambda _ k))

(define env-empty '())
(define (env-extend1 env name val) `((,name . ,val) . ,env))
(define (env-extend* env name* val*)
  (cond
    ((null? name*) env)
    ((symbol? name*) (env-extend1 env name* val*))
    (else (env-extend* (env-extend1 env (car name*) (car val*))
                       (cdr name*) (cdr val*)))))
(define (env-lookup/k env name k-bound k-unbound)
  (cond
    ((null? env) (k-unbound))
    ((equal? (car (car env)) name) (k-bound (cdr (car env))))
    (else (env-lookup/k (cdr env) name k-bound k-unbound))))
(define (env-lookup env name)
  (env-lookup/k
    env name id
    (lambda () (error 'env-lookup (format "unbound variable ~s" name)))))
(define (env-index/k env name k-bound k-unbound)
  (let loop ((env env) (idx 0))
    (cond
      ((null? env) (k-unbound))
      ((equal? (car (car env)) name) (k-bound idx))
      (else (loop (cdr env) (+ 1 idx))))))
(define (env-index env name)
  (env-index/k
    env name id
    (lambda () (error 'env-lookup (format "unbound variable ~s" name)))))
(define (env-ref env idx) (cdr (list-ref env idx)))
(define (bound? env datum)
  (env-lookup/k env datum (const #t) (const #f)))

(define (pattern-match/k hole? pat datum k-succeed k-fail)
  (if (hole? pat)
    (k-succeed datum)
    (cond
      ((and (pair? pat) (pair? datum))
       (pattern-match/k
         hole?
         (car pat)
         (car datum)
         (lambda a
           (pattern-match/k
             hole?
             (cdr pat)
             (cdr datum)
             (lambda d (apply k-succeed (append a d)))
             (lambda (failure) (k-fail (cons `(cdr ,pat ,datum) failure)))))
         (lambda (failure) (k-fail (cons `(car ,pat ,datum) failure)))))
      ((eqv? pat datum) (k-succeed))
      (else (k-fail `((_ ,pat ,datum)))))))

(define (syntax-pattern pattern datum)
  (pattern-match/k
    not pattern datum id*
    (lambda (failure) (error 'syntax-pattern "bad syntax ~s" failure))))

(define (case-pattern datum pc*-all)
  (let loop ((pc* pc*-all))
    (if (null? pc*)
      (error 'case-pattern "no match for ~s with ~s" datum (map car pc*-all))
      (pattern-match/k
        not (caar pc*) datum (cadar pc*) (lambda (_) (loop (cdr pc*)))))))
