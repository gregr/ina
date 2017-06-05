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
(define (denote-pair da dd) (lambda (env) `(,(da env) . ,(dd env))))
(define (denote-list ds)
  (let loop ((ds ds))
    (if (null? ds)
      (denote-literal '())
      (denote-pair (car ds) (loop (cdr ds))))))
(define (denote-procedure body params env)
  (let ((dbody (denote body (env-extend* env params params))))
    (lambda (env) (lambda args (dbody (env-extend* env params args))))))
(define (denote-application dproc args env)
  (let ((dargs (map (lambda (arg) (denote arg env)) args)))
    (lambda (env) (apply (dproc env) (map (lambda (darg) (darg env)) dargs)))))

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

(define (denote expr env)
  (cond
    ((or (boolean? expr) (number? expr)) (denote-literal expr))
    ((symbol? expr)
     (let ((idx (env-index env expr))) (lambda (env) (env-ref env idx))))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (denote-application (denote head env) (cdr expr) env)
         (case head
           ((quote) (denote-literal (car (syntax-pattern '(quote #f) expr))))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (denote-procedure body params env)))
           ((if)
            (let* ((parts (syntax-pattern '(if #f #f #f) expr))
                   (condition (car parts))
                   (alt-true (cadr parts))
                   (alt-false (caddr parts))
                   (dc (denote condition env))
                   (dt (denote alt-true env))
                   (df (denote alt-false env)))
              (lambda (env) (if (dc env) (dt env) (df env)))))
           ((let)  ;; TODO: optionally-named.
            (let* ((parts (syntax-pattern '(let #f #f) expr))
                   (bindings (map (lambda (b) (syntax-pattern '(#f #f) b))
                                  (car parts)))
                   (body (cadr parts))
                   (params (map car bindings))
                   (args (map cadr bindings)))
              (denote-application
                (denote-procedure body params env) args env)))
           ((quasiquote)
            (let loop ((level 0) (qq-expr (car (syntax-pattern
                                                 '(quasiquote #f) expr))))
              (case-pattern
                qq-expr
                `(((,'unquote #f)
                   ,(lambda (datum)
                      (if (= 0 level)
                        (denote datum env)
                        (denote-list (list (denote-literal 'unquote)
                                           (loop (- level 1) datum))))))
                  ((quasiquote #f)
                   ,(lambda (datum)
                      (denote-list (list (denote-literal 'quasiquote)
                                         (loop (+ level 1) datum)))))
                  ((#f . #f)
                   ,(lambda (a d) (denote-pair (loop level a) (loop level d))))
                  (#f ,denote-literal)))))
           (else (error 'denote (format "unbound variable ~s" head)))))))
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
