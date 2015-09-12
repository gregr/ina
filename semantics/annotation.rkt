#lang racket/base
(provide
  ann-empty
  annotate
  annotate-value
  )

(require
  "term.rkt"
  gregr-misc/set
  racket/match
  racket/set
  )

(define ann-empty set-empty)

(define (annotate-value ann val)
  (match val
    ((v-ann inner v) (annotate-value (set-union ann inner) v))
    ((? value?)      (v-ann ann val))))

(define annotate t-ann)
