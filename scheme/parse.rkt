#lang racket/base
(provide
  parse
  senv-empty
  senv-rename
  senv-set
  senv-ref
  )

(require
  "primitive-operation.rkt"
  "record.rkt"
  "s-term.rkt"
  racket/vector
  )

(define fresh-name
  (let ((scope 0))
    (lambda (name)
      (define sn (vector scope name))
      (set! scope (+ scope 1))
      sn)))

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

;; TODO: how should we represent primitive procedures and mvectors?
(define-record r-vector r-vector? r-vector-datum)

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
  (cond ((vector? datum) (s-literal (r-vector datum)))
        ((or (datum-atom? datum) (pair? datum)) (s-literal datum))
        (else (error 'build-literal (format "unhandled datum ~s" datum)))))
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

(define (symbol-or-false*? s*)
  (andmap (lambda (x) (or (not x) (symbol? x))) s*))
(define (unique-or-false*? x*)
  (or (null? x*)
      (and (or (not (car x*)) (not (member (car x*) (cdr x*))))
           (unique-or-false*? (cdr x*)))))
(define (param-list? p*)
  (and (list? p*) (symbol-or-false*? p*) (unique-or-false*? p*)))

(define (preparse form) (lambda (env) (parse env form)))
(define (parse* env form*) (map (lambda (e) (parse env e)) form*))

(define (parse env form)
  (define (check condition)
    (when (not condition) (error 'parse (format "invalid syntax ~s" form))))
  (define (check-binding form) (check (and (list? form) (= 2 (length form)))))

  (define (build-binder env param* id&env->binder)
    (let* ((base (fresh-name param*))
           (offset (and (list? param*) (length param*)))
           (id (s-id base offset))
           (env (if (not (list? param*)) (senv-bind env param* id)
                  (begin (check (param-list? param*))
                         (senv-bind* env param* base)))))
      (id&env->binder id env)))
  (define (build-lambda env param* env->body)
    (build-binder env param* (lambda (id env) (s-lambda id (env->body env)))))
  (define (build-let env param* targ* env->body)
    (build-apply (build-lambda env param* env->body) targ*))
  (define (build-letrec env param* env->init* env->body)
    (build-binder
      env param* (lambda (id env)
                   (s-letrec id (env->init* env) (env->body env)))))

  (define (parse-binder/k fbindings fbody k)
    (map check-binding fbindings)
    (k (map car fbindings) (map cadr fbindings) (preparse fbody)))

  (define (parse-letrec form)
    (check (= 3 (length form)))
    (parse-binder/k
      (cadr form) (caddr form)
      (lambda (param* init* pbody)
        (build-letrec env param* (lambda (env) (parse* env init*)) pbody))))

  (define (parse-let form)
    (define flen (length form))
    (define _ (check (or (= 3 flen) (= 4 flen))))
    (define name (and (= 4 flen) (cadr form)))
    (define parts (if name (cddr form) (cdr form)))
    (parse-binder/k
      (car parts) (cadr parts)
      (lambda (param* arg* pbody)
        (if name
          (build-letrec
            env (list name)
            (lambda (env) (list (build-lambda env param* pbody)))
            (lambda (env) (build-apply (parse env name) (parse* env arg*))))
          (build-let env param* (parse* env arg*) pbody)))))

  (define (parse-let* form)
    (check (= 3 (length form)))
    (parse-binder/k
      (cadr form) (caddr form)
      (lambda (param* arg* pbody)
        ((let loop ((p* param*) (a* arg*))
           (if (null? p*) pbody
             (lambda (env)
               (build-let env (list (car p*)) (list (parse env (car a*)))
                          (loop (cdr p*) (cdr a*))))))
         env))))

  (define (parse-quasiquote form)
    (define (bad msg) (error 'parse (format "bad ~a ~s" msg form)))
    (check (= 2 (length form)))
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

  (cond
    ((datum-self-evaluating? form) (build-literal form))
    ((symbol? form)                (senv-ref-variable env form))
    ((pair? form)
     (let* ((head (car form)) (keyword (senv-ref-keyword env head)))
       (if (not keyword)
         (build-apply (parse env head) (parse* env (cdr form)))
         (case keyword
           ((quote)  (check (= 2 (length form))) (build-literal (cadr form)))
           ((lambda) (check (= 3 (length form)))
                     (build-lambda env (cadr form) (preparse (caddr form))))
           ((if)     (check (= 4 (length form)))
                     (apply s-if (parse* env (cdr form))))
           ((letrec letrec*) (parse-letrec form))
           ((let)            (parse-let form))
           ((let*)           (parse-let* form))
           ((quasiquote)     (parse-quasiquote form))
           (else (error 'parse (format "unbound identifier ~s" head)))))))
    (else (error 'parse (format "invalid syntax ~s" form)))))
