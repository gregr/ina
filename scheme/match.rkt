#lang racket/base
(provide
  ;; TODO:
  )
(require
  "type.rkt"
  )

(define-variant-type
  pat?
  (pat-exist pat-exist? pat-exist-ids pat-exist-p)
  (pat-any pat-any?)
  (pat-var pat-var? pat-var-id)
  (pat-literal pat-literal? pat-literal-datum)

  (pat-cons pat-cons? pat-cons-car pat-cons-cdr)
  (pat-segment
    pat-segment? pat-segment-min-length pat-segment-p pat-segment-cdr)
  (pat-vector pat-vector? pat-vector-lp)
  (pat-syntax pat-syntax? pat-syntax-vlp)

  (pat-and pat-and? pat-and-c1 pat-and-c2)
  (pat-or pat-or? pat-or-d1 pat-or-d2)
  (pat-not pat-not? pat-not-p)

  (pat-? pat-?? pat-?-predicate)
  (pat-app pat-app? pat-app-transformer pat-app-p)
  )
