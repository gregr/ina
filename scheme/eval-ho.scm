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
(define (bound? env datum) (env-index/k env datum (const #t) (const #f)))

(define (denote-literal value) (lambda (env) value))
(define (denote-application proc args env)
  (let ((dproc (denote proc env))
        (dargs (map (lambda (arg) (denote arg env)) args)))
    (lambda (env) (apply (dproc env) (map (lambda (darg) (darg env)) dargs)))))

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

(define (denote expr env)
  (cond
    ((or (boolean? expr) (number? expr)) (denote-literal expr))
    ((symbol? expr)
     (let ((idx (env-index env expr))) (lambda (env) (env-ref env idx))))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (denote-application head (cdr expr) env)
         (case head
           ((quote) (denote-literal (car (syntax-pattern '(quote #f) expr))))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts))
                   (dbody (denote body (env-extend* env params params))))
              (lambda (env)
                (lambda args (dbody (env-extend* env params args))))))
           ((if)
            (let* ((parts (syntax-pattern '(if #f #f #f) expr))
                   (condition (car parts))
                   (alt-true (cadr parts))
                   (alt-false (caddr parts))
                   (dc (denote condition env))
                   (dt (denote alt-true env))
                   (df (denote alt-false env)))
              (lambda (env) (if (dc env) (dt env) (df env)))))
           (else (denote-application head (cdr expr) env))))))
    (else (error 'denote (format "invalid syntax ~s" expr)))))

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

(define (evaluate expr env) ((denote expr env) env))
