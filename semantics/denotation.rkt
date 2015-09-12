#lang racket/base
(provide
  denote
  env-empty
  )

(require
  "annotation.rkt"
  "substitution.rkt"
  "term.rkt"
  racket/match
  )

(module+ test
  (require rackunit))

(define env-empty '())
(define env-ref list-ref)
(define (env-add env v) (cons v env))

(define (denote-value val scope ann)
  (match val
    ((v-ann ann val) (denote-value val scope ann))
    ((v-lam body)    (let ((db (denote-term body (+ 1 scope) ann)))
                       (lambda (env) (lambda (arg) (db (env-add env arg))))))
    ((v-pair l r)    (let ((dl (denote-value l scope ann))
                           (dr (denote-value r scope ann)))
                       (lambda (env) (cons (dl env) (dr env)))))
    ((v-bit bt)      (match bt ((b-0) (lambda (env) 0))
                               ((b-1) (lambda (env) 1))))
    ((v-unit)        (lambda (env) '()))
    ((v-var idx)     (if (< idx scope) (lambda (env) (env-ref env idx))
                       (error (format "unbound variable; depth=~a index=~a; ~a"
                                      scope idx ann))))))

(define (denote-term tm scope ann)
  (match tm
    ((t-ann ann tm)   (denote-term tm scope ann))
    ((t-subst sub tm) (denote-term (substitute sub tm) scope ann))
    ((t-value val)    (denote-value val scope ann))
    ((t-unpair tbit tpair)
     (let ((dbit (denote-term tbit scope ann))
           (dpair (denote-term tpair scope ann)))
       (lambda (env)
         (match (dpair env)
           ((cons pl pr)
            (match (dbit env) (0 pl) (1 pr)
              (val (error (format "cannot unpair with non-bit ~v; depth=~a; ~a"
                                  val scope ann)))))
           (val (error (format "cannot unpair non-pair ~v; depth=~a; ~a"
                               val scope ann)))))))
    ((t-apply tproc targ)
     (let ((dproc (denote-term tproc scope ann))
           (darg (denote-term targ scope ann)))
       (lambda (env)
         (match (dproc env)
           ((? procedure? proc) (proc (darg env)))
           (val (error (format "cannot apply non-procedure ~v; depth=~a; ~a"
                               val scope ann)))))))))

(define (denote tm (scope 0) (ann ann-empty)) (denote-term tm scope ann))

(module+ test
  (define dt (denote (t-value (v-pair
                                (v-var 0) (v-pair (v-bit (b-0)) (v-unit))))
                     1))
  (check-equal? (dt (env-add env-empty 'a))
                '(a 0)))

(module+ test
  (define test-term-0
    (t-apply
      (t-value (v-lam (t-unpair
                        (t-value (v-var 0))
                        (t-value (v-pair (v-unit)
                                         (v-pair (v-var 0) (v-var 1)))))))
      (t-value (v-var 0))))
  (define test-term-1
    (t-apply (t-value (v-lam test-term-0)) (t-value (v-bit (b-1)))))
  (define completed ((denote test-term-1) env-empty))
  (check-equal? completed '(1 . 1)))
