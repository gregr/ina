#lang racket/base
(provide
  parse
  env-empty
  env-alias
  env-set-syntax
  env-set-variable
  env-ref-id
  env-ref-syntax
  env-ref-variable

  senv-empty
  senv-rename
  senv-set
  senv-ref
  )

(require
  "primitive-operation.rkt"
  "type.rkt"
  "s-term.rkt"
  racket/vector
  )

(define fresh-name
  (let ((scope 0))
    (lambda (origin)
      (define sn (vector scope origin))
      (set! scope (+ scope 1))
      sn)))

(define-type*
  binding?
  (b-syntax b-syntax? b-syntax-transformer)
  (b-variable b-variable?))

(define env-empty (hash))
(define (env-set env name id details)
  (if name (hash-set env name (cons id details)) env))
(define (env-set-variable senv name id) (env-set senv name id b-variable))
(define (env-set-syntax senv name id trans)
  (env-set senv name id (b-syntax trans)))
(define (env-ref env name) (hash-ref env name (cons name #f)))
(define (env-ref-id env name) (car (env-ref env name)))
(define (env-ref-variable env name)
  (define binding (env-ref env name))
  (if (b-variable? (cdr binding)) (car binding)
    (error 'env-ref-variable (format "unbound variable ~s" name))))
(define (env-ref-syntax env name)
  (define binding (env-ref env name))
  (and (b-syntax? (cdr binding)) (b-syntax-transformer (cdr binding))))
(define (env-alias env alias aliased)
  (hash-set env alias (env-ref env aliased)))

(define senv-empty (hash))
(define (senv-set senv name syntax) (if name (hash-set senv name syntax) senv))
(define (senv-rename senv old new) (senv-set senv old (hash-ref senv new new)))
(define (senv-bind senv param id) (senv-set senv param (s-variable id)))
(define (senv-bind* senv param* base)
  (define (vref i) (s-variable (s-id base i)))
  (let loop ((i 0) (p* param*) (senv senv))
    (if (null? p*) senv
      (loop (+ i 1) (cdr p*) (senv-set senv (car p*) (vref i))))))
(define (senv-ref senv name) (hash-ref senv name name))
(define (senv-ref-variable senv name)
  (define bindee (and (symbol? name) (senv-ref senv name)))
  (when (or (not bindee) (symbol? bindee))
    (error 'senv-ref-variable (format "unbound identifier ~s" name)))
  bindee)
(define (senv-ref-keyword senv name)
  (define bindee (and (symbol? name) (senv-ref senv name)))
  (and (symbol? bindee) bindee))

(define (datum-self-evaluating? datum)
  (or (boolean? datum) (number? datum) (char? datum) (string? datum)))
(define (datum-atom? datum)
  (or (datum-self-evaluating? datum) (symbol? datum) (null? datum)))
(define (datum-valid-literal? datum)
  (cond ((vector? datum) (andmap datum-valid-literal? (vector->list datum)))
        ((pair? datum) (and (datum-valid-literal? (car datum))
                            (datum-valid-literal? (cdr datum))))
        (else (datum-atom? datum))))

(define (build-literal datum)
  (when (not (datum-valid-literal? datum))
    (error 'build-literal (format "invalid literal datum ~s" datum)))
  (s-literal datum))
(define build-null (build-literal '()))
(define (build-pair ta td)
  (if (and (s-literal? ta) (s-literal? td))
    (build-literal (cons (s-literal-datum ta) (s-literal-datum td)))
    (s-primitive-operation (po-cons ta td))))
(define (build-list ts) (foldr build-pair build-null ts))
(define (build-vector tv)
  (define (not-literal? x) (not (s-literal? x)))
  (if (= 0 (vector-length (vector-filter not-literal? tv)))
    (build-literal (vector-map s-literal-datum tv))
    (s-primitive-operation (po-vector tv))))
(define (build-apply tproc targ*) (s-apply tproc (list->vector targ*)))
(define (build-list-append tx ty)
  (s-primitive-operation (po-list-append tx ty)))
(define (build-list->vector tx) (s-primitive-operation (po-list->vector tx)))

(define (param-list? p*)
    (or (and (null? p*) p*)
        (and (or (not p*) (symbol? p*)) (list p*))
        (and (pair? p*) (let ((pa (car p*)) (pd (param-list? (cdr p*))))
                          (and pd (not (member pa pd)) (cons pa pd))))))
(define (param-variadic? p*) (or (not p*) (and (symbol? p*) p*)
                                 (and (pair? p*) (param-variadic? (cdr p*)))))

(define (preparse form) (lambda (env) (parse env form)))
(define (parse* env form*) (map (lambda (e) (parse env e)) form*))

(define (parse env form)
  (define (check condition)
    (if (not condition) (error 'parse (format "invalid syntax ~s" form))
      condition))
  (define (check-binding form) (check (and (list? form) (= 2 (length form)))))
  (define (check-list= len form)
    (check (and (list? form) (= len (length form)))))
  (define (check-list<= len form)
    (check (and (list? form) (<= len (length form)))))

  (define (build-binder env param* id&env->binder)
    (define p* (check (param-list? param*)))
    (define variadic? (param-variadic? param*))
    (define base (fresh-name param*))
    (define offset (length p*))
    (define id (s-id base offset))
    (define binder (id&env->binder id (senv-bind* env p* base)))
    (if variadic? (s-lambda* (- offset 1) binder) binder))
  (define (build-lambda env param* env->body)
    (build-binder env param* (lambda (id env) (s-lambda id (env->body env)))))
  (define (build-let env param* targ* env->body)
    (build-apply (build-lambda env param* env->body) targ*))
  (define (build-letrec env param* env->init* env->body)
    (build-binder
      env param*
      (lambda (id env)
        (s-letrec id (list->vector (env->init* env)) (env->body env)))))

  (define (parse-define-binding form)
    (check-list<= 3 form)
    (define bound (cadr form))
    (cond ((or (not bound) (symbol? bound))
           (check (= 3 (length form))) (list bound (preparse (caddr form))))
          (else (check-list<= 1 bound)
                (list (car bound)
                      (lambda (env)
                        (build-lambda
                          env (cdr bound) (preparse-body* (cddr form))))))))

  (define (parse-body*-binding* env form*)
    (let loop ((f* form*) (env env))
      (define (continue)
        (cons (list #f (preparse (car f*))) (loop (cdr f*) env)))
      (cond ((null? f*) '())
            ((pair? (car f*))
             (define form (car f*))
             (define keyword (senv-ref-keyword env (car form)))
             (if (not keyword) (continue)
               (case keyword
                 ((begin) (check (list? (cdr form)))
                          (loop (append (cdr form) (cdr f*)) env))
                 ((define) (define binding (parse-define-binding form))
                           (define senv (senv-set env (car binding) #f))
                           (cons binding (loop (cdr f*) senv)))
                 (else (continue)))))
            (else (continue)))))

  (define (parse-body* env form*)
    (define rb* (reverse (parse-body*-binding* env form*)))
    (check-list<= 1 rb*)
    (define body (and (check (not (caar rb*))) (cadar rb*)))
    (define b* (reverse (cdr rb*)))
    (if (null? b*) (body env)
      (build-letrec env (map car b*)
                    (lambda (env) (map (lambda (b) ((cadr b) env)) b*))
                    body)))

  (define (preparse-body* form*) (lambda (env) (parse-body* env form*)))

  (define (parse-quote form) (check-list= 2 form) (build-literal (cadr form)))

  (define (parse-if form)
    (check-list= 4 form) (apply s-if (parse* env (cdr form))))

  (define (parse-lambda form)
    (check-list<= 3 form)
    (build-lambda env (cadr form) (preparse-body* (cddr form))))

  (define (parse-binder/k fbindings fbody* k)
    (map check-binding fbindings)
    (k (map car fbindings) (map cadr fbindings) (preparse-body* fbody*)))

  (define (parse-letrec form)
    (check-list<= 3 form)
    (parse-binder/k
      (cadr form) (cddr form)
      (lambda (param* init* pbody)
        (build-letrec env param* (lambda (env) (parse* env init*)) pbody))))

  (define (parse-let form)
    (check-list<= 3 form)
    (define name
      (and (symbol? (cadr form)) (check (<= 4 (length form))) (cadr form)))
    (define parts (if name (cddr form) (cdr form)))
    (parse-binder/k
      (car parts) (cdr parts)
      (lambda (param* arg* pbody)
        (if name
          (build-letrec
            env (list name)
            (lambda (env) (list (build-lambda env param* pbody)))
            (lambda (env) (build-apply (parse env name) (parse* env arg*))))
          (build-let env param* (parse* env arg*) pbody)))))

  (define (parse-let* form)
    (check-list<= 3 form)
    (parse-binder/k
      (cadr form) (cddr form)
      (lambda (param* arg* pbody)
        ((let loop ((p* param*) (a* arg*))
           (if (null? p*) pbody
             (lambda (env)
               (build-let env (list (car p*)) (list (parse env (car a*)))
                          (loop (cdr p*) (cdr a*))))))
         env))))

  (define (parse-quasiquote form)
    (define (bad msg) (error 'parse (format "bad ~a ~s" msg form)))
    (check-list= 2 form)
    (let loop ((level 0) (qqf (cadr form)))
      (cond
        ((eqv? 'unquote qqf) (bad "unquote"))
        ((eqv? 'unquote-splicing qqf) (bad "unquote-splicing"))
        ((datum-atom? qqf) (build-literal qqf))
        ((vector? qqf) (build-list->vector (loop level (vector->list qqf))))
        ((not (pair? qqf)) (check #f))
        ((eqv? 'unquote-splicing (car qqf)) (bad "unquote-splicing"))
        ((equal? '(unquote) qqf) (bad "unquote"))
        ((and (eqv? 'unquote (car qqf))
              (pair? (cdr qqf))
              (not (null? (cddr qqf)))) (bad "unquote"))
        ((eqv? 'quasiquote (car qqf))
         (build-list (list (build-literal 'quasiquote)
                           (loop (+ level 1) (cadr qqf)))))
        ((eqv? 'unquote (car qqf))
         (if (= 0 level) (parse env (cadr qqf))
           (build-list (list (build-literal 'unquote)
                             (loop (- level 1) (cadr qqf))))))
        ((and (pair? (car qqf))
              (eqv? 'unquote-splicing (caar qqf))
              (pair? (cdar qqf))
              (null? (cddar qqf)))
         (let ((td (loop level (cdr qqf))))
           (if (= 0 level) (build-list-append (parse env (cadar qqf)) td)
             (build-pair
               (build-list (list (build-literal 'unquote-splicing)
                                 (loop (- level 1) (cadar qqf))))
               td))))
        (else (build-pair (loop level (car qqf)) (loop level (cdr qqf)))))))

  (define (parse-begin form)
    (check-list<= 2 form)
    (define f* (reverse (cdr form)))
    (define result (car f*))
    (define effect* (reverse (cdr f*)))
    (s-seq (list->vector (parse* env effect*)) (parse env result)))

  (cond
    ((datum-self-evaluating? form) (build-literal form))
    ((symbol? form)                (senv-ref-variable env form))
    ((pair? form)
     (let* ((head (car form)) (keyword (senv-ref-keyword env head)))
       (if (not keyword)
         (build-apply (parse env head) (parse* env (cdr form)))
         (case keyword
           ((quote)          (parse-quote form))
           ((lambda)         (parse-lambda form))
           ((if)             (parse-if form))
           ((letrec letrec*) (parse-letrec form))
           ((let)            (parse-let form))
           ((let*)           (parse-let* form))
           ((quasiquote)     (parse-quasiquote form))
           ((begin)          (parse-begin form))
           (else (error 'parse (format "unbound identifier ~s" head)))))))
    (else (error 'parse (format "invalid syntax ~s" form)))))
