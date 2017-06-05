;; TODO: letrec quasiquote vector vector-ref vector?

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
(define (bound? env datum)
  (env-lookup/k env datum (const #t) (const #f)))

(define (evaluate-procedure body params env)
  (lambda args (evaluate body (env-extend* env params args))))
(define (evaluate-application proc args env)
  (apply proc (map (lambda (arg) (evaluate arg env)) args)))

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

(define (evaluate expr env)
  (cond
    ((or (boolean? expr) (number? expr)) expr)
    ((symbol? expr) (env-lookup env expr))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (evaluate-application (evaluate head env) (cdr expr) env)
         (case head
           ((quote) (car (syntax-pattern '(quote #f) expr)))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (evaluate-procedure body params env)))
           ((if)
            (let* ((parts (syntax-pattern '(if #f #f #f) expr))
                   (condition (car parts))
                   (alt-true (cadr parts))
                   (alt-false (caddr parts)))
              (if (evaluate condition env)
                (evaluate alt-true env)
                (evaluate alt-false env))))
           ((let)  ;; TODO: optionally-named.
            (let* ((parts (syntax-pattern '(let #f #f) expr))
                   (bindings (map (lambda (b) (syntax-pattern '(#f #f) b))
                                  (car parts)))
                   (body (cadr parts))
                   (params (map car bindings))
                   (args (map cadr bindings)))
              (evaluate-application
                (evaluate-procedure body params env) args env)))
           ((quasiquote)
            (let loop ((level 0) (qq-expr (car (syntax-pattern
                                                 '(quasiquote #f) expr))))
              (case-pattern
                qq-expr
                `(((,'unquote #f)
                   ,(lambda (datum)
                      (if (= 0 level)
                        (evaluate datum env)
                        `(,'unquote ,(loop (- level 1) datum)))))
                  ((quasiquote #f)
                   ,(lambda (datum)
                      `(,'quasiquote ,(loop (+ level 1) datum))))
                  ((#f . #f)
                   ,(lambda (a d) `(,(loop level a) . ,(loop level d))))
                  (#f ,id)))))
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
