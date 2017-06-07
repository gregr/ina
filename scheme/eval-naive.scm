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

(define (denote-reference env idx name) (env-ref env idx))
(define (denote-literal expr) expr)
(define (denote-pair da dd) `(,da . ,dd))
(define (denote-procedure body params env)
  (lambda args (denote body (env-extend* env params args))))
(define (denote-application proc args env)
  (apply proc (map (lambda (arg) (denote arg env)) args)))
(define (denote-if dc tdt tdf) (if dc (tdt) (tdf)))

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
