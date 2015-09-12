#lang racket/base
(provide
  (struct-out b-0)
  (struct-out b-1)
  bit?

  (struct-out s-ann)
  (struct-out subst)

  (struct-out t-ann)
  (struct-out t-subst)
  (struct-out t-value)
  (struct-out t-unpair)
  (struct-out t-apply)
  term?

  (struct-out v-ann)
  (struct-out v-unit)
  (struct-out v-bit)
  (struct-out v-pair)
  (struct-out v-lam)
  (struct-out v-var)
  value?
  )

(require
  gregr-misc/record
  )

(records term
  (t-ann    ann t)
  (t-subst  s t)
  (t-value  v)
  (t-unpair bit pair)
  (t-apply  proc arg))

(records value
  (v-ann  ann v)
  (v-unit)
  (v-bit  b)
  (v-pair l r)
  (v-lam  body)
  (v-var  index))

(records bit (b-0) (b-1))

(record s-ann ann v)
(record subst bindings lift)
