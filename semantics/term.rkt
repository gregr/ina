#lang racket/base
(provide
  b-0
  b-1

  subst

  t-subst
  t-value
  t-unpair
  t-apply

  v-unit
  v-bit
  v-pair
  v-lam
  v-var
  )

(require
  gregr-misc/record
  )

(records term
  (t-subst  s t)
  (t-value  v)
  (t-unpair bit pair)
  (t-apply  proc arg))

(records value
  (v-unit)
  (v-bit  b)
  (v-pair l r)
  (v-lam  body)
  (v-var  index))

(records bit (b-0) (b-1))

(record subst bindings lift)
