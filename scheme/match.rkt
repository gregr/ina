#lang racket/base
(provide
  ;; TODO:
  )
(require
  "type.rkt"
  racket/set
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

(define set-empty (set))

(define (bound-pattern-ids pat)
  (cond
    ((pat-exist? pat)
     (set-subtract (bound-pattern-ids (pat-exist-p pat)) (pat-exist-ids pat)))
    ((pat-var? pat) (set (pat-var-id pat)))
    ((pat-and? pat)     (set-union (bound-pattern-ids (pat-and-c1 pat))
                                   (bound-pattern-ids (pat-and-c2 pat))))
    ((pat-or? pat)      (set-union (bound-pattern-ids (pat-or-d1 pat))
                                   (bound-pattern-ids (pat-or-d2 pat))))
    ((pat-cons? pat)    (set-union (bound-pattern-ids (pat-cons-car pat))
                                   (bound-pattern-ids (pat-cons-cdr pat))))
    ((pat-segment? pat) (set-union (bound-pattern-ids (pat-segment-p pat))
                                   (bound-pattern-ids (pat-segment-cdr pat))))
    ((pat-vector? pat)  (bound-pattern-ids (pat-vector-lp pat)))
    ((pat-syntax? pat)  (bound-pattern-ids (pat-syntax-vlp pat)))
    ((pat-app? pat)     (bound-pattern-ids (pat-app-p pat)))
    (else set-empty)))
