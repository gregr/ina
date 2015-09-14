#lang racket/base
(provide
  denote
  env-empty
  )

(require
  "substitution.rkt"
  "term.rkt"
  racket/format
  racket/match
  )

(module+ test
  (require rackunit))

(define env-empty '())
(define env-ref list-ref)
(define (env-add env v) (cons v env))

(define (denote tm (annotate ~a) (scope 0) (path '()))
  (define (denote-value val scope path)
    (match val
      ((v-lam body) (let ((db (denote-term
                                body (+ 1 scope) (list* 'body path))))
                      (lambda (env) (lambda (arg) (db (env-add env arg))))))
      ((v-pair l r) (let ((dl (denote-value l scope (list* 'l path)))
                          (dr (denote-value r scope (list* 'r path))))
                      (lambda (env) (cons (dl env) (dr env)))))
      ((v-bit bt)   (match bt ((b-0) (lambda (env) 0))
                              ((b-1) (lambda (env) 1))))
      ((v-unit)     (lambda (env) '()))
      ((v-var idx)  (if (< idx scope) (lambda (env) (env-ref env idx))
                      (error (format "unbound variable; depth=~a index=~a; ~a"
                                     scope idx (annotate path)))))))
  (define (denote-term tm scope path)
    (match tm
      ((t-subst sub tm) (denote-term (substitute sub tm) scope path))
      ((t-value val)    (denote-value val scope (list* 'v path)))
      ((t-unpair tbit tpair)
       (let ((dbit (denote-term tbit scope (list* 'bit path)))
             (dpair (denote-term tpair scope (list* 'pair path))))
         (lambda (env)
           (match (dpair env)
             ((cons pl pr)
              (match (dbit env) (0 pl) (1 pr)
                (val (error
                       (format "cannot unpair with non-bit ~v; depth=~a; ~a"
                               val scope (annotate path))))))
             (val (error (format "cannot unpair non-pair ~v; depth=~a; ~a"
                                 val scope (annotate path))))))))
      ((t-apply tproc targ)
       (let ((dproc (denote-term tproc scope (list* 'proc path)))
             (darg (denote-term targ scope (list* 'arg path))))
         (lambda (env)
           (match (dproc env)
             ((? procedure? proc) (proc (darg env)))
             (val (error (format "cannot apply non-procedure ~v; depth=~a; ~a"
                                 val scope (annotate path))))))))))
  (denote-term tm scope path))


(module+ test
  (define dt (denote (t-value (v-pair
                                (v-var 0) (v-pair (v-bit (b-0)) (v-unit))))
                     ~a 1))
  (check-equal? (dt (env-add env-empty 'a))
                '(a 0)))

(module+ test
  (require
    "operation.rkt"
    gregr-misc/maybe
    )
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
  (check-equal? ((denote (just-x (step test-term-1))) env-empty) completed)
  (check-equal? completed '(1 . 1)))
