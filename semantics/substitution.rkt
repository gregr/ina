#lang racket/base
(provide
  apply->subst
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
    (match (list-ref bindings idx)
      ((s-ann _ v) v)
      (v v))
    (v-var (+ lift (- idx binding-count)))))

(define v0 (v-var 0))
(define sub-lift-1 (subst '() 1))

(define (substitute-value sub val)
  (match val
    ((v-pair l r)  (apply-map* v-pair (curry substitute-value sub) l r))
    ((v-var index) (substitute-var sub index))
    ((v-lam body)
     (lets (subst bindings lift) = (substitute-subst sub-lift-1 sub)
           (v-lam (t-subst (subst (list* v0 bindings) lift) body))))
    ((? value?)    val)))

(define (apply->subst proc arg (ann #f))
  (match proc
    ((s-ann ann proc) (apply->subst proc arg ann))
    ((v-lam body)     (subst (list (if ann (s-ann ann arg) arg)) 0) body)))

(define (substitute sub tm)
  (define sub-value (curry substitute-value sub))
  (match tm
    ((t-subst inner tm) (t-subst (substitute-subst sub inner) tm))
    ((t-value val)      (t-value (sub-value val)))
    ((t-unpair idx pr)  (apply-map* t-unpair sub-value idx pr))
    ((t-apply proc arg) (apply-map* t-apply (curry t-subst sub) proc arg))))
