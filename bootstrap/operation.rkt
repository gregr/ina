#lang racket/base
(provide
  drive
  step
  step-complete
  step-continuation
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

(define (drive max-depth max-span tm (depth 0) (span 0))
  (define (drive-value full? depth span val)
    (match val
      ((v-unit)       val)
      ((v-bit  b)     val)
      ((v-var  index) val)
      ((v-pair l r)
       (if full? (apply-map* v-pair (curry drive-value full? depth span) l r)
         val))
      ((v-lam  body)
       (if (and full? (not (and max-depth (<= max-depth depth))))
         (v-lam (drive-term #t (+ 1 depth) 0 body))
         val))))
  (define (drive-value-subst full? depth span s v)
    (match v
      ((v-var _) (substitute-value s v))
      ((v-pair l r)
       (apply-map* v-pair (curry drive-value-subst full? depth span s) l r))
      (_ (drive-value full? depth span (substitute-value s v)))))
  (define (drive-term full? depth span tm)
    (define self (curry drive-term full? depth span))
    (match tm
      ((t-value  v) (t-value (drive-value full? depth span v)))
      ((t-dsubst dsub tm) (self (dsubst->t-subst dsub tm)))
      ((t-subst  s t)
       (match t
         ((t-value v) (t-value (drive-value-subst full? depth span s v)))
         (_           (self (substitute s t)))))
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
          (check-equal? (drive 0 0 tt) tc)
          (check-equal? (drive #f #f tt) tn)
          ))
  (check-equal?
    (drive 1 100 test-omega 1)
    test-omega)
  (define completed (step-complete test-term-1))
  (check-match
    (step test-term-1)
    (just _))
  (check-match
    (step completed)
    (nothing))
  )
