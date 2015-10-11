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

(define (drive tm max-depth max-span (depth 0) (span 0))
  (define (drive-value full? depth span val)
    (match val
      ((v-unit)       val)
      ((v-bit  b)     val)
      ((v-var  index) val)
      ((v-pair l r)   (apply-map*
                        v-pair (curry drive-value full? depth span) l r))
      ((v-lam  body)  (if full? (v-lam (drive-term #t (+ 1 depth) 0 body))
                        val))))
  (define (drive-term full? depth span tm)
    (define self (curry drive-term full? depth span))
    (match tm
      ((t-value  v) (t-value (drive-value full? depth span v)))
      ((t-subst  s t)
       (match t
         ((t-value (v-var _)) (substitute s t))
         (_                   (self (substitute s t)))))
      ((t-unpair bit pair)
       (match* ((self bit) (self pair))
         (((t-value (v-bit bt)) (t-value (v-pair p0 p1)))
          (t-value (match bt ((b-0) p0) ((b-1) p1))))
         ((t0 t1) (t-unpair t0 t1))))
      ((t-apply  proc arg)
       (let* ((t0 (drive-term #f depth span proc))
              (t1 (drive-term #t depth span arg))
              (span (if (= 0 depth) span (+ span 1)))
              (self (curry drive-term full? depth span)))
         (if (or (and max-span (< 0 depth) (<= max-span span))
                 (and max-depth (< max-depth depth)))
           (t-apply (if full? (self t0) t0) t1)
           (match* (t0 t1)
             (((t-value (v-lam body)) (t-value varg))
              (self (t-subst (subst-single varg) body)))
             ((t0 t1) (t-apply (if full? (self t0) t0) t1))))))))
  (drive-term #t depth span tm))

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
  (define terms*completes*normal0s*normals
    `((,test-term-1 ,test-complete-1 ,test-complete-1 ,test-complete-1)
      (,test-term-2 ,test-term-2 ,test-term-2 ,test-term-2)
      (,test-term-3 ,test-term-3 ,test-normalized-3 ,test-normalized-3)
      (,test-term-4 ,test-complete-4 ,test-complete-4 ,test-complete-4)
      (,test-term-5 ,test-term-5 ,test-term-5 ,test-normalized-5)
      ))
  (for_ (list tt tc tn0 tn) <- terms*completes*normal0s*normals
        (begin
          (check-equal? (step-complete tt) tc)
          (check-equal? (drive tt 0 0) tn0)
          (check-equal? (drive tt #f #f) tn)
          ))
  (define completed (step-complete test-term-1))
  (check-match
    (step test-term-1)
    (just _))
  (check-match
    (step completed)
    (nothing))
  )
