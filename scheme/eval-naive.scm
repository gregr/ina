;; TODO: aim for self-applicability.
;; Do letrec, named let, begin/define last
;; begin/define should define error-producing procs first, then let-bound non-procs, then letrec procs
;; Do these first:
;; cond, case, and, or
;;
;; list, list-ref, list-tail, null?, boolean?, length, map, reverse
;; caar, cadr, cdar, cadar, caddr, cadddr
;; define equal? in terms of something like eqv?
;; identity issues: equality of pairs, vectors, procedures
;; optional (would have to remove uses of these):
;; error, strings, format
;; let*, letrec, cond, case, and, or
;; named let, begin/define, define within lambda and let
;; =, +, -
(load "common.scm")

(define (vector-reify v)
  (if (vector? v)
    v
    (error 'vector-reify (format "invalid vector ~s" v))))

(define (denote-reference env addr name) (env-ref env addr))
(define (denote-literal expr) expr)
(define (denote-vector ds) (apply vector ds))
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
      (= . ,=)
      (boolean=? . ,boolean=?)
      (symbol=? . ,symbol=?)
      (null? . ,null?)
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
