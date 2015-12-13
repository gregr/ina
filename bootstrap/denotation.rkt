#lang racket/base
(provide
  denote
  )

(require
  "substitution.rkt"
  "term.rkt"
  gregr-misc/sugar
  racket/format
  racket/match
  )

(module+ test
  (require rackunit))

(define env-empty '())
(define env-ref list-ref)
(define (env-add env v) (cons v env))
(define (env-extend env vs) (append vs env))

(define (pre-denote tm annotate scope path)
  (def (pre-denote-subst pdbody (subst bindings lift) body body-key)
    (values dbindings _) =
    (forf result = '() sub-path = (list* 'first path)
          val <- bindings
          (values (list* (pre-denote-value
                           val scope (list* 's 'bindings sub-path)) result)
                  (list* 'rest sub-path)))
    dbody = (pdbody body (+ (length bindings) scope) (list* body-key path))
    (lambda (env)
      (dbody (env-extend env (reverse (forl dval <- dbindings (dval env)))))))
  (define (pre-denote-value val scope path)
    (match val
      ((v-subst sub val) (pre-denote-subst pre-denote-value sub val 'v))
      ((v-lam body) (let ((db (pre-denote-term
                                body (+ 1 scope) (list* 'body path))))
                      (lambda (env) (lambda (arg) (db (env-add env arg))))))
      ((v-pair l r) (let ((dl (pre-denote-value l scope (list* 'l path)))
                          (dr (pre-denote-value r scope (list* 'r path))))
                      (lambda (env) (cons (dl env) (dr env)))))
      ((v-bit bt)   (match bt ((b-0) (lambda (env) 0))
                              ((b-1) (lambda (env) 1))))
      ((v-unit)     (lambda (env) '()))
      ((v-var idx)  (if (< idx scope) (lambda (env) (env-ref env idx))
                      (error (format "unbound variable; depth=~s index=~s; ~s"
                                     scope idx (annotate path)))))))
  (define (pre-denote-term tm scope path)
    (match tm
      ((t-subst sub tm) (pre-denote-subst pre-denote-term sub tm 't))
      ((t-value val)    (pre-denote-value val scope (list* 'v path)))
      ((t-unpair tbit tpair)
       (let ((dbit (pre-denote-term tbit scope (list* 'bit path)))
             (dpair (pre-denote-term tpair scope (list* 'pair path))))
         (lambda (env)
           (match (dpair env)
             ((cons pl pr)
              (match (dbit env) (0 pl) (1 pr)
                (val (error
                       (format "cannot unpair with non-bit ~v; depth=~s; ~s"
                               val scope (annotate path))))))
             (val (error (format "cannot unpair non-pair ~v; depth=~s; ~s"
                                 val scope (annotate path))))))))
      ((t-apply tproc targ)
       (let ((dproc (pre-denote-term tproc scope (list* 'proc path)))
             (darg (pre-denote-term targ scope (list* 'arg path))))
         (lambda (env)
           (let ((proc (dproc env)) (arg (darg env)))
             (match proc
               ((? procedure? proc) (proc arg))
               (val (error
                      (format
                        "cannot apply non-procedure ~v to ~v; depth=~s; ~s"
                        val arg scope (annotate path)))))))))))
  (pre-denote-term tm scope path))

(define (denote term (annotate ~s))
  ((pre-denote term annotate 0 '()) env-empty))

(module+ test
  (define dt
    (pre-denote (t-value (v-pair (v-var 0) (v-pair (v-bit (b-0)) (v-unit))))
                ~s 1 '()))
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
  (define completed (denote test-term-1))
  (check-equal? (denote (just-x (step test-term-1))) completed)
  (check-equal? completed '(1 . 1)))
