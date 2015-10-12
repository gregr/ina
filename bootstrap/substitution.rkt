#lang racket/base
(provide
  dsubst->t-subst
  subst-single
  substitute
  substitute-full
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
(def (undefer-dsubst sub)
  (subst bindings lift) = (substitute-subst sub-lift-1 sub)
  (subst (list* v0 bindings) lift))
(def (complete-dsubst sub dsub)
  (subst bindings lift) = dsub
  (match sub
    ((subst (list a0) 0) (subst (list* a0 bindings) lift))
    (_ (substitute-subst sub (undefer-dsubst dsub)))))
(define (dsubst->t-subst dsub tm) (t-subst (undefer-dsubst dsub) tm))

(define (substitute-value sub val)
  (match val
    ((v-pair l r)  (apply-map* v-pair (curry substitute-value sub) l r))
    ((v-var index) (substitute-var sub index))
    ((v-lam body)  (v-lam (t-dsubst sub body)))
    ((? value?)    val)))

(define (substitute sub tm)
  (match tm
    ((t-dsubst dsub tm) (t-subst (complete-dsubst sub dsub) tm))
    ((t-subst inner tm) (t-subst (substitute-subst sub inner) tm))
    ((t-value val)      (t-value (substitute-value sub val)))
    ((t-unpair idx pr)  (apply-map* t-unpair (curry t-subst sub) idx pr))
    ((t-apply proc arg) (apply-map* t-apply (curry t-subst sub) proc arg))))

(define (substitute-full tv)
  (match tv
    ((t-dsubst sub tm)  (substitute-full (dsubst->t-subst sub tm)))
    ((t-subst sub tm)   (substitute-full (substitute sub tm)))
    ((t-value v)        (t-value (substitute-full v)))
    ((t-unpair idx pr)  (apply-map* t-unpair substitute-full idx pr))
    ((t-apply proc arg) (apply-map* t-apply substitute-full proc arg))
    ((v-pair l r)       (apply-map* v-pair substitute-full l r))
    ((v-lam body)       (v-lam (substitute-full body)))
    ((? value?)         tv)))
