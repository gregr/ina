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
      ((v-pair l r) (apply-map* v-pair (curry drive-value #t depth span) l r))
      ((v-lam  body)
       (if (and full? (not (and max-depth (<= max-depth depth))))
         (v-lam (drive-term #t (+ 1 depth) span body))
         val))))
  (define (drive-value-subst full? depth span s v)
    (match v
      ((v-var _) (substitute-value s v))
      ((v-pair l r)
       (apply-map* v-pair (curry drive-value-subst #t depth span s) l r))
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
       (let* ((span-next (if (= 0 depth) span (+ span 1)))
              (exceeded? (and max-span (< 0 depth) (<= max-span span-next)))
              (t1 (drive-term #t depth span arg))
              (varg (match t1 ((t-value varg) varg) (_ #f)))
              (stop? (or exceeded? (not varg)))
              (t0 (drive-term stop? depth span proc))
              (body (and (not stop?)
                         (match t0 ((t-value (v-lam body)) body) (_ #f)))))
         (if body (drive-term full? depth span-next
                              (t-subst (subst-single varg) body))
           (t-apply t0 t1))))))
  (drive-term #t depth span tm))

; TODO: off by one errors everywhere?
(define (compress tm)
  (record memo-ref rib idx)
  (record memo-frame depth tiers)
  (define rib-min (list 0 -1))
  (def (rib-max (list d0 t0) (list d1 t1))
       (if (= d0 d1) (list d0 (max t0 t1))
         (if (< d0 d1) (list d1 t1) (list d0 t0))))
  (define (rib-max-above depth tv)
    (define (rma2 tv0 tv1)
      (rib-max (rib-max-above depth tv0) (rib-max-above depth tv1)))
    (match tv
      ((memo-ref rib idx)
       (lets (list rd rt) = rib (if (<= rd depth) rib rib-min)))
      ((v-unit)     rib-min)
      ((v-bit  b)   rib-min)
      ((v-var  idx) rib-min)
      ((v-pair l r) (rma2 l r))
      ((v-lam body) (rib-max-above (- depth 1) body))
      ((t-value v)  (rib-max-above depth v))
      ((t-dsubst dsub tm) (rib-max-above depth (dsubst->t-subst dsub tm)))
      ((t-subst (subst bindings k) t)
       (forf mrib = (rib-max-above depth t)
             rib <- (map (curry rib-max-above depth) bindings)
             (rib-max mrib rib)))
      ((t-unpair bit pair) (rma2 bit pair))
      ((t-apply proc arg) (rma2 proc arg))))

  (define memo-empty (list (memo-frame 0 '())))
  (define (memo-push memo)
    (list* (memo-frame (+ 1 (memo-frame-depth (car memo))) '()) memo))
  (def (cmref< (cons _ (memo-ref _ i0)) (cons _ (memo-ref _ i1))) (< i0 i1))
  (def (memo-pop (cons (memo-frame depth tiers) memo) tm)
       tm = (forf tm = tm tier <- tiers
                  bindings = (map car (sort (hash->list tier) cmref<))
                  (t-subst (subst bindings 0) tm))
       (values memo tm))
  (def (memoize memo val)
       (cons (memo-frame fd ft) _) = memo
       (list depth tier) = (rib-max-above fd val)
       tier-target = (+ 1 tier)
       memo-idx = (- fd depth)
       (memo-frame _ tiers) = (list-ref memo memo-idx)
       tiers = (if (<= (length tiers) tier-target) (list* (hash) tiers) tiers)
       tier-idx = (- (length tiers) tier-target)
       tier = (list-ref tiers tier-idx)
       ref-next = (memo-ref (list depth tier-target) (hash-count tier))
       ref = (hash-ref-default tier val ref-next)
       tier = (hash-set tier val ref)
       tiers = (list-set tiers tier-idx tier)
       memo = (list-set memo memo-idx (memo-frame depth tiers))
       (values memo ref))
  (def (memo-map f memo ctor args)
    (values memo acc) =
    (forf memo = memo acc = '() arg <- args
          (values memo result) = (f memo arg)
          (values memo (list* result acc)))
    (values memo (apply ctor (reverse acc))))

  (define (ref->var stack tv)
    (match tv
      ((memo-ref (list depth tdepth) idx)
       (lets tier-offsets = (list-ref stack (- (length stack) depth))
             tier-offset = (list-ref tier-offsets (- (length tiers) tdepth))
             (v-var (+ tier-offset idx))))
      ((v-unit)     tv)
      ((v-bit  b)   tv)
      ((v-var  idx) tv)
      ((v-pair l r) (apply-map* v-pair (curry ref->var stack) l r))
      ((v-lam body) (lets offset = (car (car stack))
                          stack = (list* '() stack)
                          (v-lam (ref->var stack body))))
      ((t-value v) (t-value (ref->var stack v)))
      ((t-subst (subst bindings 0) t)
       (lets (cons top rest) = stack
             stack = (list* (list* (+ (length bindings)
                                      (if (pair? top) (car top) 0)) top) rest)
             (t-subst (subst bindings 0) (ref->var stack t))))
      ((t-unpair bt pr) (apply-map* t-unpair (curry ref->var stack) bt pr))
      ((t-apply pc arg) (apply-map* t-apply (curry ref->var stack) pc arg))))
  (define (compress-value memo val)
    (match val
      ((memo-ref rib idx) (values memo val))
      ((v-unit)           (values memo val))
      ((v-bit  b)         (values memo val))
      ((v-var  idx) (values memo (memo-ref (list (- (length memo) idx) -1) 0)))
      ((v-pair l r)
       (lets (values memo vp) = (memo-map compress-value memo v-pair l r)
             (memoize memo vp)))
      ((v-lam body)
       (lets (values memo body) = (compress-term (memo-push memo) body)
             (values memo body) = (memo-pop memo body)
             (memoize memo (v-lam body))))))
  (define (compress-term memo tm)
    (match tm
      ((t-value v) (memo-map compress-value memo t-value v))
      ((t-dsubst dsub tm) (compress-term memo (dsubst->t-subst dsub tm)))
      ((t-subst (subst bindings k) t)
       (lets (values memo bs) = (memo-map compress-value memo list bindings)
             (compress-term memo (substitute (subst bs k) t))))
      ((t-unpair bit pair) (memo-map compress-term memo t-unpair bit pair))
      ((t-apply proc arg) (memo-map compress-term memo t-apply proc arg))))
  (lets (values memo tm) = (compress-term memo-empty tm)
        (values _ tm) = (memo-pop memo tm)
        (ref->var '(()) tm)))

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
