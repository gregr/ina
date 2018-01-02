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
  )

(define fresh-name
  (let ((scope 0))
    (lambda (name)
      (define sn (vector scope name))
      (set! scope (+ scope 1))
      sn)))

(define senv-empty (hash))
(define (senv-set senv name syntax) (hash-set senv name syntax))
(define (senv-rename senv old new) (senv-set senv old (hash-ref senv new new)))
(define (senv-lambda senv param id) (senv-set senv param (s-variable id)))
(define (senv-lambda* senv param* id)
  (define v (s-variable id))
  (define (vref i) (s-primitive-operation (po-vector-ref v i)))
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
(define (symbol-list? s*) (and (list? s*) (andmap symbol? s*)))
(define (unique-list? x*)
  (or (null? x*)
      (and (not (member (car x*) (cdr x*))) (unique-list? (cdr x*)))))
(define (param-list? p*) (and (symbol-list? p*) (unique-list? p*)))

(define (build-literal datum)
  (when (not (datum-valid-literal? datum))
    (error 'build-literal (format "invalid literal datum ~s" datum)))
  (cond ((vector? datum) (s-literal (r-vector datum)))
        ((or (datum-atom? datum) (pair? datum)) (s-literal datum))
        (else (error 'build-literal (format "unhandled datum ~s" datum)))))
(define build-null (build-literal '()))
(define (build-pair ta td) (s-primitive-operation (po-cons ta td)))
(define (build-list ts) (foldr build-pair build-null ts))
(define (build-vector ts) (s-primitive-operation (po-vector ts)))
(define (build-apply* tproc targ*)
  (s-apply tproc (s-primitive-operation (po-vector (list->vector targ*)))))

(define (preparse form) (lambda (env) (parse env form)))
(define (parse* env form*) (map (lambda (e) (parse env e)) form*))
(define (parse env form)
  (define (check condition)
    (when (not condition) (error 'parse (format "invalid syntax ~s" form))))
  (define (check-binding form) (check (and (list? form) (= 2 (length form)))))
  (define (build-lambda env param* env->tbody)
    (let* ((id (fresh-name param*))
           (env (if (symbol? param*)
                  (senv-lambda env param* id)
                  (begin (check (param-list? param*))
                         (senv-lambda* env param* id)))))
      (s-lambda id (env->tbody env))))
  (define (build-let env param* targ* env->body)
    (build-apply* (build-lambda env param* env->body) targ*))
  (define (build-letrec env param* env->arg* env->body)
    (error 'build-letrec "TODO"))
  (cond
    ((datum-self-evaluating? form) (build-literal form))
    ((symbol? form) (senv-ref-variable env form))
    ((pair? form)
     (let* ((head (car form)) (keyword (senv-ref-keyword env head)))
       (if (not keyword)
         (build-apply* (parse env head) (parse* env (cdr form)))
         (case keyword
           ((quote) (check (= 2 (length form))) (build-literal (cadr form)))
           ((lambda) (check (= 3 (length form)))
                     (build-lambda env (cadr form) (preparse (caddr form))))
           ((if) (check (= 4 (length form)))
                 (apply s-if (parse* env (cdr form))))
           ((let)
            (let* ((flen (length form))
                   (_ (check (or (= 3 flen) (= 4 flen))))
                   (name (and (= 4 flen) (cadr form)))
                   (parts (if name (cddr form) (cdr form)))
                   (bindings (and (map check-binding (car parts)) (car parts)))
                   (pbody (preparse (cadr parts)))
                   (param* (map car bindings))
                   (arg* (map cadr bindings)))
              (if name
                (build-letrec
                  env (list name)
                  (lambda (env) (list (build-lambda env param* pbody)))
                  (lambda (env)
                    (build-apply* (parse env name) (parse* env arg*))))
                (build-let env param* (parse* env arg*) pbody))))
           ((let*)
            (check (= 3 (length form)))
            (let* ((bindings (and (map check-binding (cadr form)) (cadr form)))
                   (pbody (preparse (caddr form)))
                   (param* (map car bindings))
                   (arg* (map cadr bindings)))
              ((let loop ((p* param*) (a* arg*))
                 (if (null? p*)
                   pbody
                   (lambda (env)
                     (build-let env (list (car p*)) (list (parse env (car a*)))
                                (loop (cdr p*) (cdr a*))))))
               env)))
           (else (error 'parse (format "unbound identifier ~s" head)))))))
    (else (error 'parse (format "invalid syntax ~s" form)))))
