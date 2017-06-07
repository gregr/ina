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
    (lambda () (error 'env-index (format "unbound variable ~s" name)))))
(define (env-ref env idx) (cdr (list-ref env idx)))
(define (bound? env datum) (env-index/k env datum (const #t) (const #f)))

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

;; Required definitions:
;;   denote-reference
;;   denote-literal
;;   denote-pair
;;   denote-procedure
;;   denote-application
;;   denote-if
(define (denote-list ds)
  (let loop ((ds ds))
    (if (null? ds)
      (denote-literal '())
      (denote-pair (car ds) (loop (cdr ds))))))
(define (denote expr env)
  (cond
    ((or (boolean? expr) (number? expr)) (denote-literal expr))
    ((symbol? expr) (denote-reference env (env-index env expr) expr))
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
            (let* ((parts (syntax-pattern '(if #f #f #f) expr)))
              (denote-if (denote (car parts) env)
                         (lambda () (denote (cadr parts) env))
                         (lambda () (denote (caddr parts) env)))))
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
                  (,'unquote
                    ,(lambda _ (error 'denote-quasiquote
                                      (format "bad unquote ~s" expr))))
                  ((,'unquote)
                   ,(lambda _ (error 'denote-quasiquote
                                     (format "bad unquote ~s" expr))))
                  ((,'unquote #f #f . #f)
                   ,(lambda _ (error 'denote-quasiquote
                                     (format "bad unquote ~s" expr))))
                  ((#f . #f)
                   ,(lambda (a d) (denote-pair (loop level a) (loop level d))))
                  (#f ,denote-literal)))))
           (else (error 'denote (format "unbound variable ~s" head)))))))
    (else (error 'denote (format "invalid syntax ~s" expr)))))
