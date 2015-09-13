#lang racket/base
(provide
  subst-single
  substitute
  )

(require
  "term.rkt"
  gregr-misc/list
  gregr-misc/sugar
  racket/function
  racket/list
  racket/match
  )

(def (substitute-lift (subst bindings k) count)
  binding-count = (length bindings)
  drop-count = (min count binding-count)
  lift-count = (max 0 (- count binding-count))
  (subst (drop bindings drop-count) (+ k lift-count)))

(def (substitute-subst outer (subst inner-bindings inner-lift))
  bindings-prefix = (map (curry substitute-value outer) inner-bindings)
  (subst bindings-suffix lift) = (substitute-lift outer inner-lift)
  (subst (append bindings-prefix bindings-suffix) lift))

(def (substitute-var (subst bindings lift) idx)
  binding-count = (length bindings)
  (if (< idx binding-count)
    (list-ref bindings idx)
    (v-var (+ lift (- idx binding-count)))))

(define v0 (v-var 0))
(define sub-lift-1 (subst '() 1))
(define (subst-single arg) (subst (list arg) 0))

(define (substitute-value sub val)
  (match val
    ((v-pair l r)  (apply-map* v-pair (curry substitute-value sub) l r))
    ((v-var index) (substitute-var sub index))
    ((v-lam body)
     (lets (subst bindings lift) = (substitute-subst sub-lift-1 sub)
           (v-lam (t-subst (subst (list* v0 bindings) lift) body))))
    ((? value?)    val)))

(define (substitute sub tm)
  (match tm
    ((t-subst inner tm) (t-subst (substitute-subst sub inner) tm))
    ((t-value val)      (t-value (substitute-value sub val)))
    ((t-unpair idx pr)  (apply-map* t-unpair (curry t-subst sub) idx pr))
    ((t-apply proc arg) (apply-map* t-apply (curry t-subst sub) proc arg))))
