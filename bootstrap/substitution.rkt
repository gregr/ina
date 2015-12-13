#lang racket/base
(provide
  dsubst->t-subst
  subst-empty
  subst-extend
  substitute
  substitute-full
  substitute-value
  )

(require
  "term.rkt"
  gregr-misc/list
  gregr-misc/sugar
  racket/function
  racket/list
  racket/match
  )

(module+ test
  (require rackunit))

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
(define subst-empty (subst '() 0))
(define subst-lift-1 (subst '() 1))
(def (subst-extend (subst bindings k) arg) (subst (list* arg bindings) k))
(def (undefer-dsubst sub)
  (subst bindings lift) = (substitute-subst subst-lift-1 sub)
  (subst (list* v0 bindings) lift))
(def (complete-dsubst sub dsub)
  (subst bindings lift) = dsub
  (match sub
    ((subst (list a0) 0) (subst-extend dsub a0))
    (_ (substitute-subst sub (undefer-dsubst dsub)))))
(define (dsubst->t-subst dsub tm) (t-subst (undefer-dsubst dsub) tm))

(define (substitute-value sub val)
  (match val
    ((v-subst _ _) (v-subst sub val))
    ((v-lam _)     (v-subst sub val))
    ((v-pair l r)  (apply-map* v-pair (curry substitute-value sub) l r))
    ((v-var index) (substitute-var sub index))
    ((? value?)    val)))

(define (substitute sub tm)
  (match tm
    ((t-dsubst dsub tm) (t-subst (complete-dsubst sub dsub) tm))
    ((t-subst inner tm) (t-subst (substitute-subst sub inner) tm))
    ((t-value val)      (t-value (substitute-value sub val)))
    ((t-unpair idx pr)  (apply-map* t-unpair (curry t-subst sub) idx pr))
    ((t-apply proc arg) (apply-map* t-apply (curry t-subst sub) proc arg))))

(define (substitute-full tv)
  (define (self sub tv)
    (match tv
      ((t-dsubst inner t) (self sub (dsubst->t-subst inner t)))
      ((t-subst inner t) (self (if sub (substitute-subst sub inner) inner) t))
      ((t-value v) (t-value (self sub v)))
      ((t-unpair idx pr) (apply-map* t-unpair (curry self sub) idx pr))
      ((t-apply proc arg) (apply-map* t-apply (curry self sub) proc arg))
      ((v-subst inner v) (self (if sub (substitute-subst sub inner) inner) v))
      ((v-pair l r) (apply-map* v-pair (curry self sub) l r))
      ((v-lam body) (v-lam (self #f (if sub (t-dsubst sub body) body))))
      ((v-var index) (if sub (self #f (substitute-var sub index)) tv))
      ((? value?) tv)))
  (self #f tv))

(module+ test
  (check-equal?
    (substitute-full
      (t-subst (subst (list (v-pair (v-unit) (v-lam (t-value (v-unit))))
                            (v-bit (b-1))) 0)
        (t-apply (t-value (v-lam (t-unpair (t-value (v-var 1))
                                           (t-value (v-var 2)))))
                 (t-value (v-bit (b-0))))))
    (t-apply (t-value (v-lam (t-unpair (t-value (v-pair
                                                  (v-unit)
                                                  (v-lam (t-value (v-unit)))))
                                       (t-value (v-bit (b-1))))))
             (t-value (v-bit (b-0)))))
  )
