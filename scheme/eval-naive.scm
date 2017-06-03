;; TODO: letrec quasiquote vector vector-ref vector?

(define (id v) v)
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
(define (bound? env datum)
  (env-lookup/k env datum (const #t) (const #f)))

(define (evaluate-application proc args env)
  (apply (evaluate proc env)
         (map (lambda (arg) (evaluate arg env)) args)))

(define (pattern-match/k hole? pat datum k-succeed k-fail)
  (if (hole? pat)
    (k-succeed (list datum))
    (cond
      ((and (pair? pat) (pair? datum))
       (pattern-match/k
         hole?
         (car pat)
         (car datum)
         (lambda (a)
           (pattern-match/k
             hole?
             (cdr pat)
             (cdr datum)
             (lambda (d) (k-succeed (append a d)))
             (lambda (failure) (k-fail (cons `(cdr ,pat ,datum) failure)))))
         (lambda (failure) (k-fail (cons `(car ,pat ,datum) failure)))))
      ((eqv? pat datum) (k-succeed '()))
      (else (k-fail `((_ ,pat ,datum)))))))

(define (syntax-pattern pattern datum)
  (pattern-match/k
    not pattern datum id
    (lambda (failure) (error 'syntax-pattern "bad syntax ~s" failure))))

(define (evaluate expr env)
  (cond
    ((or (boolean? expr) (number? expr)) expr)
    ((symbol? expr) (env-lookup env expr))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (evaluate-application head (cdr expr) env)
         (case head
           ((quote) (car (syntax-pattern '(quote #f) expr)))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (lambda args (evaluate body (env-extend* env params args)))))
           ((if)
            (let* ((parts (syntax-pattern '(if #f #f #f) expr))
                   (condition (car parts))
                   (alt-true (cadr parts))
                   (alt-false (caddr parts)))
              (if (evaluate condition env)
                (evaluate alt-true env)
                (evaluate alt-false env))))
           (else (error 'evaluate (format "unbound variable ~s" head)))))))
    (else (error 'evaluate (format "invalid syntax ~s" expr)))))

(define env-initial
  `((cons . ,cons)
    (car . ,car)
    (cdr . ,cdr)
    (not . ,not)
    (equal? . ,equal?)
    (pair? . ,pair?)
    (symbol? . ,symbol?)
    (number? . ,number?)
    (procedure? . ,procedure?)
    (apply . ,apply)
    . ,env-empty))
