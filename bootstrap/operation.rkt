#lang racket/base
(provide
  step
  step-complete
  )

(require
  "substitution.rkt"
  "term.rkt"
  gregr-misc/cursor
  gregr-misc/list
  gregr-misc/maybe
  racket/function
  racket/match
  )

(module+ test
  (require
    gregr-misc/sugar
    rackunit
    ))

(define (step-continuation-downward cterm)
  (define (sub-cont choice)
    (step-continuation-downward (::@ cterm (list choice))))
  (match (::.* cterm)
    ((? t-subst?)  (just cterm))
    ((? t-value?)  (nothing))
    ((? t-unpair?) (maybe-or (sub-cont 'bit) (sub-cont 'pair) (just cterm)))
    ((? t-apply?)  (maybe-or (sub-cont 'proc) (sub-cont 'arg) (just cterm)))))

(define (step-continuation-upward-with key cterm)
  (match* (key (::.* cterm))
    (('bit  (? t-unpair?)) (just (::@ cterm '(pair))))
    (('pair (? t-unpair?)) (just cterm))
    (('proc (? t-apply?))  (just (::@ cterm '(arg))))
    (('arg  (? t-apply?))  (just cterm))
    ((_     _)             (step-continuation-upward cterm))))

(define (step-continuation-upward cterm)
  (match (cursor-trail cterm)
    ('()          (nothing))
    ((cons key _) (step-continuation-upward-with key (::^ cterm)))))

(define (step-continuation cterm)
  (maybe-or
    (step-continuation-downward cterm)
    (maybe-fold1 step-continuation (step-continuation-upward cterm))))

(define (step-execute cterm)
  (maybe-map
    (curry ::=* cterm)
    (match (::.* cterm)
      ((t-dsubst dsub tm) (just (dsubst->t-subst dsub tm)))
      ((t-subst sub tm) (just (substitute sub tm)))
      ((t-unpair (t-value (v-bit bt)) (t-value (v-pair p0 p1)))
       (just (t-value (match bt ((b-0) p0) ((b-1) p1)))))
      ((t-apply (t-value (v-lam body)) (t-value arg))
       (just (t-subst (subst-single arg) body)))
      ((? term?) (nothing)))))

(define (step-once cterm) (maybe-fold1 step-execute (step-continuation cterm)))
(define step-full (curry maybe-iterate step-once))

(define (step term) (maybe-map ::^*. (step-once (::0 term))))
(define step-complete (compose1 ::^*. step-full ::0))

(module+ test
  (define test-term-0
    (t-apply
      (t-value
        (v-lam (t-unpair
                 (t-value (v-var 0))
                 (t-value (v-pair (v-unit) (v-pair (v-var 0) (v-var 1)))))))
      (t-value (v-var 0))))
  (define test-term-1
    (t-apply (t-value (v-lam test-term-0)) (t-value (v-bit (b-1)))))
  (define test-complete-1 (t-value (v-pair (v-bit (b-1)) (v-bit (b-1)))))
  (define test-term-2
    (t-value (v-pair (v-bit (b-1))
                     (v-lam (t-value (v-pair (v-bit (b-1)) (v-bit (b-0))))))))
  (define test-term-3
    (t-value (v-pair (v-bit (b-1))
                     (v-lam (t-unpair (t-value (v-bit (b-1)))
                                      (t-value (v-pair (v-bit (b-1))
                                                       (v-bit (b-0)))))))))
  (define test-normalized-3
    (t-value (v-pair (v-bit (b-1)) (v-lam (t-value (v-bit (b-0)))))))
  (define test-term-4
    (t-apply
      (t-value (v-lam (t-unpair (t-value (v-var 0))
                                (t-value (v-pair (v-bit (b-1))
                                                 (v-bit (b-0)))))))
      (t-value (v-bit (b-1)))))
  (define test-complete-4 (t-value (v-bit (b-0))))
  (define test-term-5 (t-value (v-lam test-term-4)))
  (define test-normalized-5 (t-value (v-lam test-complete-4)))
  (define test-term-6 (t-apply test-term-5 (t-value (v-unit))))
  (define test-complete-6 test-complete-4)
  (define test-term-7
    (t-apply
      (t-value (v-lam (t-value (v-lam (t-unpair (t-value (v-var 1))
                                                (t-value (v-pair
                                                           (v-bit (b-1))
                                                           (v-bit (b-0)))))))))
      (t-value (v-bit (b-1)))))
  (define test-complete-7
    (t-value
      (v-lam (t-dsubst (subst (list (v-bit (b-1))) 0)
                       (t-unpair (t-value (v-var 1))
                                 (t-value (v-pair
                                            (v-bit (b-1))
                                            (v-bit (b-0)))))))))
  (define test-normalized-7 (t-value (v-lam (t-value (v-bit (b-0))))))
  (define test-omega
    (t-apply
      (t-value (v-lam (t-apply (t-value (v-var 0)) (t-value (v-var 0)))))
      (t-value (v-lam (t-apply (t-value (v-var 0)) (t-value (v-var 0)))))))
  (define terms*completes*normals
    `((,test-term-1 ,test-complete-1 ,test-complete-1)
      (,test-term-2 ,test-term-2 ,test-term-2)
      (,test-term-3 ,test-term-3 ,test-normalized-3)
      (,test-term-4 ,test-complete-4 ,test-complete-4)
      (,test-term-5 ,test-term-5 ,test-normalized-5)
      (,test-term-6 ,test-complete-6 ,test-complete-6)
      (,test-term-7 ,test-complete-7 ,test-normalized-7)
      ))
  (for_ (list tt tc tn) <- terms*completes*normals
        (begin
          (check-equal? (step-complete tt) tc)
          ))
  (define completed (step-complete test-term-1))
  (check-match
    (step test-term-1)
    (just _))
  (check-match
    (step completed)
    (nothing))
  )
