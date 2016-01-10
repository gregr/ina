#lang racket/base
(provide
  step
  step-complete
  step*
  strip-annotations
  )

(require
  "substitution.rkt"
  "term.rkt"
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/sugar
  racket/match
  )

(module+ test
  (require
    rackunit
    ))

(define (step* step-count term)
  (define (step-execute step-count term)
    (define step-count-next (and step-count (- step-count 1)))
    (define (step-apply sub arg body)
      (step* step-count-next (t-subst (subst-extend sub arg) body)))
    (define (step-unpair sub bt p0 p1)
      (define px (match bt ((b-0) p0) ((b-1) p1)))
      (step* step-count-next (t-value (if sub (substitute-value sub px) px))))
    (match term
      ((t-subst  sub tm) (step* step-count-next (substitute sub tm)))
      ((t-unpair (annotated _ (t-value (v-bit bt)))
                 (annotated _ (t-value (v-subst sub (v-pair p0 p1)))))
       (step-unpair sub bt p0 p1))
      ((t-unpair (annotated _ (t-value (v-bit bt)))
                 (annotated _ (t-value (v-pair p0 p1))))
       (step-unpair #f bt p0 p1))
      ((t-apply (annotated _ (t-value (v-subst sub (v-lam body))))
                (annotated aarg (t-value arg)))
       (step-apply sub (annotated aarg arg) body))
      ((t-apply (annotated _ (t-value (v-lam body)))
                (annotated aarg (t-value arg)))
       (step-apply subst-empty (annotated aarg arg) body))
      ((t-value (annotated ann val))
       (step* step-count (annotated ann (t-value val))))
      ((t-value (v-subst sub (annotated ann val)))
       (step* step-count (annotated ann (t-value (v-subst sub val)))))
      ((t-value (v-subst sub (? v-subst? val)))
       (step* step-count-next (t-value (substitute-value sub val))))
      ((? term?) (values step-count term))))
  (def (step-seq-execute k t0 t1)
    (values sc0 t0) = (step* step-count t0)
    (values sc1 t1) = (step* sc0 t1)
    t0 = (if (annotated? t0) t0 (annotated #f t0))
    t1 = (if (annotated? t1) t1 (annotated #f t1))
    tm = (k t0 t1)
    (if (eq? 0 sc1) (values 0 tm) (step-execute sc1 tm)))
  (if (eq? 0 step-count) (values 0 term)
    (match term
      ((annotated #f tm) (step* step-count tm))
      ((annotated ann tm)
       (lets (values sc tm) = (step* step-count tm)
             (values sc (match tm
                          ((and (annotated (not #f) _) tm) tm)
                          (tm (annotated ann tm))))))
      ((t-unpair t0 t1) (step-seq-execute t-unpair t0 t1))
      ((t-apply t0 t1) (step-seq-execute t-apply t0 t1))
      (_ (step-execute step-count term)))))

(def (step term)
     (values remaining result) = (step* 1 term)
     (if (= 0 remaining) (just result) (nothing)))
(def (step-complete term) (values _ result) = (step* #f term) result)

(define (strip-annotations tv)
  (match tv
    ((annotated _ tv) (strip-annotations tv))
    ((t-subst (subst bs k) t)
     (t-subst (subst (map strip-annotations bs) k) (strip-annotations t)))
    ((t-value v) (t-value (strip-annotations v)))
    ((t-unpair bit pair) (apply-map* t-unpair strip-annotations bit pair))
    ((t-apply  proc arg) (apply-map* t-apply strip-annotations proc arg))
    ((v-subst (subst bs k) v)
     (v-subst (subst (map strip-annotations bs) k) (strip-annotations v)))
    ((v-pair l r) (apply-map* v-pair strip-annotations l r))
    ((v-lam body) (v-lam (strip-annotations body)))
    (_ tv)))

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
      (v-subst (subst (list (v-bit (b-1))) 0)
               (v-lam (t-unpair (t-value (v-var 1))
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
          (check-equal? (strip-annotations (step-complete tt)) tc)
          ))
  (define completed (step-complete test-term-1))
  (check-match
    (step test-term-1)
    (just _))
  (check-match
    (step completed)
    (nothing))
  )
