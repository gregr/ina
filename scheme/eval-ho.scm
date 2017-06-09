(load "common.scm")

(define (vector-reify v)
  (if (vector? v)
    v
    (error 'vector-reify (format "invalid vector ~s" v))))

(define (denote-reference env addr name) (lambda (env) (env-ref env addr)))
(define (denote-literal value) (lambda (env) value))
(define (denote-pair da dd) (lambda (env) `(,(da env) . ,(dd env))))
(define (denote-procedure body params env)
  (let ((dbody (denote body (env-extend* env params params))))
    (lambda (env) (lambda args (dbody (env-extend* env params args))))))
(define (denote-application dproc dargs)
  (lambda (env) (apply (dproc env) (map (lambda (darg) (darg env)) dargs))))
(define (denote-if dc tdt tdf)
  (let ((dt (tdt)) (df (tdf))) (lambda (env) (if (dc env) (dt env) (df env)))))

(define env-initial
  (env-extend-bindings
    env-empty
    `((cons . ,cons)
      (car . ,car)
      (cdr . ,cdr)
      (not . ,not)
      (equal? . ,equal?)
      (pair? . ,pair?)
      (symbol? . ,symbol?)
      (number? . ,number?)
      (procedure? . ,procedure?)
      (vector? . ,vector?)
      (vector . ,vector)
      (vector-length . ,vector-length)
      (vector-ref . ,vector-ref)
      (apply . ,apply))))

(define (evaluate expr env) ((denote expr env) env))
