#lang racket/base
(provide
  var?
  var=?
  failure?
  failure-reasons
  failures-none!
  failures-all!
  failures-deepest!
  set-failure-merge!
  fail
  goal/context
  succeed
  ==
  fresh
  conde
  define-relation
  query
  run
  run*
  ss-map
  ss-force
  ss-take
  )

(require
  "syntax.rkt"
  "type.rkt"
  racket/vector
  )

(define-type failure failure? failure-depth failure-reasons)
(define failure-merge #f)
(define (set-failure-merge! merge) (set! failure-merge merge))

(define (failure-append depth fa fb)
  (failure depth (append (failure-reasons fa) (failure-reasons fb))))
(define (failure-merge/keep-all fa fb) (failure-append 0 fa fb))
(define (failure-merge/keep-deepest fa fb)
  (define da (failure-depth fa))
  (define db (failure-depth fb))
  (cond ((> da db) fa)
        ((< da db) fb)
        (else (failure-append da fa fb))))

(define (failures-none!) (set-failure-merge! #f))
(define (failures-deepest!) (set-failure-merge! failure-merge/keep-deepest))
(define (failures-all!) (set-failure-merge! failure-merge/keep-all))

(define (failure/ignore ss)
  (cond ((null? ss) '())
        ((failure? ss) '())
        ((procedure? ss) (lambda () (failure/ignore (ss))))
        (else (cons (car ss) (failure/ignore (cdr ss))))))
(define (failure/context ctx ss)
  (if failure-merge
    (cond ((null? ss) '())
          ((failure? ss)
           (failure (+ 1 (failure-depth ss))
                    (list (list ctx (failure-reasons ss)))))
          ((procedure? ss) (lambda () (failure/context ctx (ss))))
          (else (cons (car ss) (failure/ignore (cdr ss)))))
    (failure/ignore ss)))
(define (failure/best-reason current ss)
  (cond ((null? ss) current)
        ((failure? ss) (failure-merge current ss))
        ((procedure? ss) (lambda () (failure/best-reason current (ss))))
        (else ss)))

(define (succeed st) (list st))
(define (mplus sa sb)
  (cond ((null? sa) sb)
        ((failure? sa) (if failure-merge (failure/best-reason sa sb) sb))
        ((procedure? sa) (lambda () (mplus (if (procedure? sb) (sb) sb) sa)))
        (else (cons (car sa) (mplus sb (cdr sa))))))
(define (bind ss c)
  (cond ((null? ss) '())
        ((failure? ss) ss)
        ((procedure? ss) (lambda () (bind (ss) c)))
        (else (mplus (c (car ss)) (lambda () (bind (cdr ss) c))))))

(define (ss-map f ss)
  (define (loop succeeded? ss)
    (cond ((null? ss) '())
          ((failure? ss) (if succeeded? '() ss))
          ((procedure? ss) (lambda () (loop succeeded? (ss))))
          ((pair? ss) (cons (f (car ss)) (loop #t (cdr ss))))))
  (loop #f ss))
(define (ss-force ss) (if (procedure? ss) (ss-force (ss)) ss))
(define (ss-take n ss)
  (define (loop succeeded? n ss)
    (cond ((eqv? n 0) '())
          ((null? ss) '())
          ((failure? ss) (if succeeded? '() ss))
          ((procedure? ss) (loop succeeded? n (ss)))
          ((pair? ss) (cons (car ss) (loop #t (and n (- n 1)) (cdr ss))))
          (else ss)))
  (loop #f n ss))

(define-type var var? var-index)
(define (var=? va vb) (= (var-index va) (var-index vb)))

(define-type state state? (state-vi state-vi-set) (state-bs state-bs-set))
(define state-empty (state 0 '()))
(define (state-fresh st k)
  (define vi (state-vi st))
  ((k (var vi)) (state-vi-set st (+ 1 vi))))
(define (state-ref st vr)
  (define kv (assoc vr (state-bs st)))
  (if kv (cdr kv) vr))
(define (state-set st vr val)
  (state-bs-set st (cons (cons vr val) (state-bs st))))

(define (walk st tm)
  (cond ((var? tm) (let ((tn (state-ref st tm)))
                     (if (and (var? tn) (var=? tm tn)) tm (walk st tn))))
        ((and (syntax? tm) (var? (syntax->outer-datum tm)))
         (walk st (syntax->outer-datum tm)))
        (else tm)))
(define (walk* st t)
  (define tm (walk st t))
  (cond ((pair? tm) (cons (walk* st (car tm)) (walk* st (cdr tm))))
        ((vector? tm) (vector-map (lambda (t) (walk* st t)) tm))
        (else tm)))
(define (occurs? st vr t)
  (define tm (walk st t))
  (cond ((pair? tm) (or (occurs? st vr (car tm)) (occurs? st vr (cdr tm))))
        ((vector? tm) (occurs? st vr (vector->list tm)))
        (else (and (var? tm) (var=? vr tm)))))
(define (assign st vr val)
  (and (not (occurs? st vr val)) (state-set st vr val)))

(define (unify st a b)
  (define ta (walk st a))
  (define tb (walk st b))
  (cond ((or (eqv? ta tb)
             (and (string? ta) (string? tb) (string=? ta tb))
             (and (var? ta) (var? tb) (var=? ta tb)))
         st)
        ((var? ta) (assign st ta tb))
        ((var? tb) (assign st tb ta))
        ((and (pair? ta) (pair? tb))
         (define st1 (unify st (car ta) (car tb)))
         (and st1 (unify st1 (cdr ta) (cdr tb))))
        ((and (vector? ta) (vector? tb))
         (and (= (vector-length ta) (vector-length tb))
              (unify st (vector->list ta) (vector->list tb))))
        ((and (syntax? ta) (syntax? tb))
         (if (and (identifier? ta) (identifier? tb))
           (and (free-identifier=? ta tb) st)
           (unify st (syntax->outer-datum ta) (syntax->outer-datum tb))))
        (else #f)))

(define (fail . reason)
  (lambda (st)
    (if failure-merge
      (failure 1 (list (list (list->vector (walk* st reason)))))
      '())))

(define (== ta tb)
  (lambda (st)
    (define st1 (unify st ta tb))
    (if st1
      (succeed st1)
      ((fail "not equal:" ta tb) st))))
(define (conj2 ca cb) (lambda (st) (bind (ca st) cb)))
(define (disj2 ca cb) (lambda (st) (mplus (ca st) (lambda () (cb st)))))
(define (goal/context ctx c)
  (lambda (st) (failure/context (walk* st ctx) (c st))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) g)
    ((_ gs ... g) (conj2 (conj+ gs ...) g))))
(define-syntax fresh
  (syntax-rules ()
    ((_ () gs ...) (conj+ gs ...))
    ((_ (vname vnames ...) gs ...)
     (lambda (st)
       (state-fresh st (lambda (vname) (fresh (vnames ...) gs ...)))))))
(define-syntax conde
  (syntax-rules ()
    ((_ (gs ...)) (conj+ gs ...))
    ((_ (gs ...) cs ...) (disj2 (conj+ gs ...) (conde cs ...)))))
(define-syntax define-relation
  (syntax-rules ()
    ((_ (signature ...) body ... goal)
     (define (signature ...) (lambda (st) (lambda () body ... (goal st)))))))
(define-syntax query
  (syntax-rules ()
    ((_ (vnames ...) gs ...)
     (state-fresh
       state-empty
       (lambda (vinitial)
         (lambda (st)
           (ss-map
             (lambda (st) (walk* st vinitial))
             ((fresh (vnames ...) (== vinitial (list vnames ...))
                gs ...) st))))))))
(define-syntax run
  (syntax-rules () ((_ n body ...) (ss-take n (query body ...)))))
(define-syntax run* (syntax-rules () ((_ body ...) (run #f body ...))))
