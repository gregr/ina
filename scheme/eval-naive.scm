;; TODO: aim for self-applicability.
;; vector, vector-ref, vector?, strings
;; let*, letrec, cond, case, and, or
;; named let, begin/define, define within lambda and let
;; define equal? in terms of something like eqv?, be careful about procedures
;; gensym, error, format, list, list-ref, null?, boolean?, length, map, =, +, -,
;; caar, cadr, cdar, cadar, caddr, cadddr
;; effect/identity issues: gensyms, equality of pairs, vectors, procedures
;; flatten environment representation, strip/gc closure environments
(load "common.scm")

(define (vector-reify v)
  (if (vector? v)
    v
    (error 'vector-reify (format "invalid vector ~s" v))))

(define (denote-reference env addr name) (env-ref env addr))
(define (denote-literal expr) expr)
(define (denote-pair da dd) `(,da . ,dd))
(define (denote-procedure pdbody params env)
  (lambda args (pdbody (env-extend* env params args))))
(define (denote-application dproc dargs) (apply dproc dargs))
(define (denote-if dc tdt tdf) (if dc (tdt) (tdf)))

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

(define (evaluate expr env) (denote expr env))
