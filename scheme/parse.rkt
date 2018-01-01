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

(define (parse* env form*) (map (lambda (e) (parse env e)) form*))
(define (parse env form)
  (define (check condition)
    (when (not condition) (error 'parse (format "invalid syntax ~s" form))))
  (cond
    ((datum-self-evaluating? form) (build-literal form))
    ((symbol? form) (senv-ref-variable env form))
    ((pair? form)
     (let* ((head (car form)) (keyword (senv-ref-keyword env head)))
       (if (not keyword)
         (s-apply (parse env head)
                  (s-primitive-operation (po-vector (parse* env (cdr form)))))
         (case keyword
           ((quote) (check (= 2 (length form))) (build-literal (cadr form)))
           ((lambda) (check (= 3 (length form)))
                     (let* ((param* (cadr form))
                            (body (caddr form))
                            (id (fresh-name param*))
                            (env (if (symbol? param*)
                                   (senv-lambda env param* id)
                                   (begin (check (symbol-list? param*))
                                          (senv-lambda* env param* id)))))
                       (s-lambda id (parse env body))))
           ((if) (check (= 4 (length form)))
                 (apply s-if (parse* env (cdr form))))
           (else (error 'parse (format "unbound identifier ~s" head)))))))
    (else (error 'parse (format "invalid syntax ~s" form)))))
