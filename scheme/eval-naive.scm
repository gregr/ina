;; TODO: aim for self-applicability.
;; vector, vector-ref, vector?, strings
;; let*, letrec, cond, case, and, or
;; named let, begin/define, define within lambda and let
;; define equal? in terms of something like eqv?, be careful about procedures
;; gensym, error, format, list, list-ref, null?, boolean?, length, map, =, +, -,
;; caar, cadr, cdar, cadar, caddr, cadddr
;; effect/identity issues: gensyms, equality of pairs, vectors, procedures
;; factor out 'denote' and other utilities
;; flatten environment representation, strip/gc closure environments
(load "common.scm")

(define (denote-procedure body params env)
  (lambda args (denote body (env-extend* env params args))))
(define (denote-application proc args env)
  (apply proc (map (lambda (arg) (denote arg env)) args)))

(define (denote expr env)
  (cond
    ((or (boolean? expr) (number? expr)) expr)
    ((symbol? expr) (env-lookup env expr))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (denote-application (denote head env) (cdr expr) env)
         (case head
           ((quote) (car (syntax-pattern '(quote #f) expr)))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (denote-procedure body params env)))
           ((if)
            (let* ((parts (syntax-pattern '(if #f #f #f) expr))
                   (condition (car parts))
                   (alt-true (cadr parts))
                   (alt-false (caddr parts)))
              (if (denote condition env)
                (denote alt-true env)
                (denote alt-false env))))
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
                        `(,'unquote ,(loop (- level 1) datum)))))
                  ((quasiquote #f)
                   ,(lambda (datum)
                      `(,'quasiquote ,(loop (+ level 1) datum))))
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
                   ,(lambda (a d) `(,(loop level a) . ,(loop level d))))
                  (#f ,id)))))
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

(define (evaluate expr env) (denote expr env))
