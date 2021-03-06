(define (id v) v)
(define (id* . v) v)
(define (const k) (lambda _ k))

(define (list-foldl f acc xs)
  (if (null? xs) acc (list-foldl f (f (car xs) acc) (cdr xs))))
(define (list-foldr f acc xs)
  (if (null? xs) acc (f (car xs) (list-foldr f acc (cdr xs)))))

(define (frame-info idx single?) (cons idx single?))
(define (frame-info-idx fi) (car fi))
(define (frame-info-single? fi) (cdr fi))
(define (address eidx fi) (cons eidx fi))
(define (address-eidx addr) (car addr))
(define (address-fi addr) (cdr addr))

(define (frame-new param* value*) (cons param* value*))
(define (frame-param* frame) (car frame))
(define (frame-value* frame) (cdr frame))
(define (frame-ref x* fi)
  (let ((idx (frame-info-idx fi)))
    (if (frame-info-single? fi) (list-ref x* idx) (list-tail x* idx))))
(define (frame-value frame fi) (frame-ref (frame-value* frame) fi))
(define (bindings-new param* val*)
  (cond
    ((null? param*) '())
    ((symbol? param*) `((,param* . ,val*)))
    (else `((,(car param*) . ,(car val*))
            . ,(bindings-new (cdr param*) (cdr val*))))))
(define (bindings->frame bindings)
  (frame-new (map car bindings) (map cdr bindings)))

(define env-empty '())
(define (env-extend-bindings env bindings)
  `(,(bindings->frame bindings) . ,env))
(define (env-extend* env param* val*) `(,(frame-new param* val*) . ,env))
(define (env-extend-param* env param*) (env-extend* env param* param*))
(define (env-address/k env name k-bound k-unbound)
  (let loop-frame ((env env) (ei 0))
    (if (null? env) (k-unbound)
      (let ((frame (car env)))
        (let loop-name ((param* (frame-param* frame)) (fi 0))
          (cond
            ((null? param*) (loop-frame (cdr env) (+ 1 ei)))
            ((symbol? param*)
             (if (equal? name param*)
               (k-bound (address ei (frame-info fi #f)))
               (loop-frame (cdr env) (+ 1 ei))))
            ((equal? name (car param*))
             (k-bound (address ei (frame-info fi #t))))
            (else (loop-name (cdr param*) (+ fi 1)))))))))
(define (env-address env name)
  (env-address/k
    env name id
    (lambda () (error 'env-address (format "unbound variable ~s" name)))))
(define (env-ref env address)
  (frame-value (list-ref env (address-eidx address)) (address-fi address)))
(define (bound? env datum) (env-address/k env datum (const #t) (const #f)))

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
;;   denote-vector
;;   denote-pair
;;   denote-procedure
;;   denote-application
;;   denote-if
(define (denote-list ds)
  (let loop ((ds ds))
    (if (null? ds)
      (denote-literal '())
      (denote-pair (car ds) (loop (cdr ds))))))
(define (denote* expr* env) (map (lambda (e) (denote e env)) expr*))
(define (denote-let pdbody param* arg* env)
  (denote-application (denote-procedure pdbody param* env) (denote* arg* env)))
(define (denote-quoted form)
  (cond
    ((pair? form)
     (denote-pair (denote-quoted (car form)) (denote-quoted (cdr form))))
    ((vector? form) (denote-vector (map denote-quoted (vector->list form))))
    ((or (boolean? form) (number? form) (symbol? form) (null? form))
     (denote-literal form))
    (else (error 'denote-quoted (format "invalid quoted form ~s" form)))))
(define (denote expr env)
  (cond
    ((or (boolean? expr) (number? expr)) (denote-quoted expr))
    ((symbol? expr) (denote-reference env (env-address env expr) expr))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (denote-application (denote head env) (denote* (cdr expr) env))
         (case head
           ((quote) (denote-quoted (car (syntax-pattern '(quote #f) expr))))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (denote-procedure (pdenote body) params env)))
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
              (denote-let (pdenote body) params args env)))
           ((let*)
            (let* ((parts (syntax-pattern '(let* #f #f) expr))
                   (bindings (map (lambda (b) (syntax-pattern '(#f #f) b))
                                  (car parts)))
                   (body (cadr parts))
                   (params (map car bindings))
                   (args (map cadr bindings)))
              ((let loop ((p* params) (a* args))
                 (if (null? p*)
                   (pdenote body)
                   (pdenote-let (loop (cdr p*) (cdr a*))
                                (list (car p*))
                                (list (car a*)))))
               env)))
           ((quasiquote)
            (let loop ((level 0) (qq-expr (car (syntax-pattern
                                                 '(quasiquote #f) expr))))
              (if (vector? qq-expr)
                (denote-vector
                  (map (lambda (element) (loop level element))
                       (vector->list qq-expr)))
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
                    (#f ,denote-literal))))))
           (else (error 'denote (format "unbound variable ~s" head)))))))
    (else (error 'denote (format "invalid syntax ~s" expr)))))

(define (pdenote expr) (lambda (env) (denote expr env)))
(define (pdenote-let pdbody param* arg*)
  (lambda (env) (denote-let pdbody param* arg* env)))
