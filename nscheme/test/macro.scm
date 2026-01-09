(mdefine at-least-one-test-failed? #f)
(mdefine verbosity 1)
(define (show-compiled-racket?) (and #t (< 0 verbosity)))

(define (error:parse c) (vector 'error:parse c))
(define (error:eval  c) (vector 'error:eval  c))
(define (error:parse? x) (and (vector? x) (= (vector-length x) 2) (eqv? (vector-ref x 0) 'error:parse)))
(define (error:eval?  x) (and (vector? x) (= (vector-length x) 2) (eqv? (vector-ref x 0) 'error:eval)))

(define (test-eval env stx)
  (define (work-safely escape work)
    (current-panic-handler
     (lambda x* (escape (apply vector 'panic x*)))
     work))
  (when (< 0 verbosity)
    (displayln "EXPRESSION:")
    (pretty-write stx))
  (call/escape
   (lambda (c)
     (when (< 0 verbosity)
       (displayln "PARSE ERROR:")
       (pretty-write c))
     (error:parse c))
   (lambda (escape)
     (let ((E (work-safely escape (lambda () (parse-expression env stx)))))
       (when (< 2 verbosity)
         (displayln "PARSED:")
         (pretty-write E))
       (when (< 1 verbosity)
         (displayln "PRETTY PARSED:")
         (pretty-write (E-pretty E)))
       (when (show-compiled-racket?)
         (displayln "EQUIVALENT RACKET CODE:")
         (pretty-write (E-compile-rkt E '())))
       (call/escape
        (lambda (c)
          (when (< 0 verbosity)
            (displayln "EVALUATION ERROR:")
            (pretty-write c))
          (error:eval c))
        (lambda (escape)
          (case-values (work-safely escape (lambda () (E-eval E)))
            ((result) (when (< 0 verbosity)
                        (displayln "VALUE:")
                        (pretty-write result))
                      result)
            (result*  (when (< 0 verbosity)
                        (displayln "VALUES:")
                        (pretty-write result*))
                      `(values . ,result*)))))))))

(define (display-border)
  (displayln "================================================================================"))

(define (test-evaluation env . test-part**)
  (mdefine test-successes '())
  (mdefine test-failures  '())
  (for-each
   (lambda (test-part*)
     (when (< 0 verbosity)
       (newline)
       (display-border)
       (pretty-write (car test-part*))
       (display-border))
     (let loop ((test-part* (cdr test-part*)))
       (unless (null? test-part*)
         (unless (and (pair? test-part*) (pair? (cdr test-part*)) (pair? (cddr test-part*)))
           (mistake 'test-evaluation "not a test case" test-part*))
         (let ((test-case (list (car test-part*) (cadr test-part*) (caddr test-part*))))
           (unless (eqv? (cadr test-case) '==>)
             (mistake 'test-evaluation "test case without ==>" test-case))
           (when (< 0 verbosity) (newline))
           (when (< 2 verbosity) (pretty-write test-case))
           (let* ((stx.expr (car test-case))
                  (actual   (test-eval env stx.expr))
                  (expected (caddr test-case)))
             (cond
               ((or (and (eqv? expected 'error:parse) (error:parse? actual))
                    (and (eqv? expected 'error:eval)  (error:eval?  actual))
                    (equal? expected actual))
                (set! test-successes (cons test-case test-successes)))
               (else (set! test-failures (cons (cons actual test-case) test-failures))
                     (unless (< 0 verbosity) (newline) (pretty-write test-case))
                     (displayln "FAILED:")
                     (displayln "EXPECTED:")
                     (pretty-write expected)
                     (displayln "ACTUAL:")
                     (pretty-write actual)))))
         (loop (cdddr test-part*)))))
   test-part**)
  (let* ((tests-succeeded (length test-successes))
         (tests-failed    (length test-failures))
         (tests-total     (+ tests-succeeded tests-failed))
         (tests-passed    (- tests-total tests-failed)))
    (when (< 0 tests-failed) (set! at-least-one-test-failed? #t))
    (newline)
    (unless (= tests-passed tests-total)
      (display-border)
      (display "Tests failed: ")
      (write tests-failed)
      (display " out of ")
      (writeln tests-total)
      (display-border)
      (for-each (lambda (failure) (apply (lambda (actual expr type expected)
                                           (newline)
                                           (pretty-write `(Expected: ,expr ,type ,expected))
                                           (pretty-write `(Actual: ,actual)))
                                         failure))
                (reverse test-failures))
      (newline))
    (display-border)
    (display "Tests passed: ")
    (write tests-passed)
    (display " out of ")
    (writeln tests-total)
    (display-border)))

;; Examples for other metaprogramming facilities
;(current-environment)
;(let ()
;  (define-in-vocabulary
;   thing 'expression
;   (lambda (env stx)
;     ($quote (vector 'thing-syntax: stx))))
;  (vector 'thing: thing))
;(let ()
;  (define-in-vocabulary
;   thing 'expression
;   (lambda (env stx)
;     ($quote (vector 'thing-syntax: stx))))
;  (vector 'thing: (thing 1 2 3)))

;; Example of unhygienic expansion leading to an incorrect result.
;(let ()
;  (define-in-vocabulary
;   example-or
;   'expression
;   (lambda (env stx)
;     (parse-expression
;      env
;      (match (syntax->list stx)
;        ((list _ a b)
;         `(let ((tmp ,a))
;            (if tmp
;                tmp
;                ,b)))
;        (x (raise-parse-error "bad syntax" x))))))
;  (let ((if #f)
;        (tmp 5))
;    (example-or if tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hygienic macro expansion examples ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env.test (alist-ref library=>env 'large))
(test-evaluation
  env.test
 '(macro-hygiene
    (let ()
      (define-syntax-rule (or a b)
        (let ((tmp a))
          (if tmp
              tmp
              b)))
      (let ((if #f)
            (tmp 5))
        (or if tmp)))
    ;==>
    ;(let ((tmp_i if_u))
    ;  (if_i tmp_i
    ;        tmp_i
    ;        tmp_u))
    ;EQUIVALENT RACKET CODE:
    ;((case-lambda
    ;  (()
    ;   ((case-lambda
    ;     ((if.0 tmp.1) ((case-lambda ((tmp.2) (if tmp.2 tmp.2 tmp.1))) if.0)))
    ;    (quote #f)
    ;    (quote 5)))))
    ==> 5

   (let ((x 'not-42))
     (define-syntax-rule (bar)
       (define x 42))
     (bar)
     x)
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda ((x.0) (letrec ((x.1 (quote 42))) x.0))) (quote not-42))
   ==> not-42

   (mlet ((cell #f))
     (define x 'bad)
     (define-syntax-rule (bar stmt)
       (begin
         (define x 42)
         stmt
         (set! cell x)))
     (let ()
       (bar (define y 5))
       (vector
         cell
         y
         x)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  ((cell.0)
   ;   (letrec ((x.1 (quote bad)))
   ;     ((case-lambda
   ;       (()
   ;        (letrec ((x.2 (quote 42)) (y.3 (quote 5)))
   ;          (apply/values
   ;           (case-lambda
   ;            (X.4
   ;             ((quote #<procedure:vector>)
   ;              ((quote #<procedure:mvector-ref>) cell.0 (quote 0))
   ;              y.3
   ;              x.1)))
   ;           ((quote #<procedure:mvector-set!>) cell.0 (quote 0) x.2)))))))))
   ; ((quote #<procedure:make-mvector>) (quote 1) (quote #f)))
   ==> #(42 5 bad)

   (let ()
     (define x 'bad)
     (define-syntax-rule (bar baz)
       (begin
         (define x 'good)
         (define-syntax-rule (baz)
           x)))
     (let ()
       (bar baz)
       (baz)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (()
   ;   (letrec ((x.0 (quote bad)))
   ;     ((case-lambda (() (letrec ((x.1 (quote good))) x.1))))))))
   ==> good

   (let ()
     (define x 'bad)
     (define y 'outer)
     (let ()
       (define-syntax-rule (bar arg)
         (begin
           (define y 'inner)
           (define arg 'good)
           x))
       (bar x)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (()
   ;   (letrec ((x.0 (quote bad)) (y.1 (quote outer)))
   ;     ((case-lambda
   ;       (() (letrec ((y.2 (quote inner)) (x.3 (quote good))) x.3))))))))
   ==> good

   (let ()
     (define x 5)
     (let ()
       (define-syntax m
         (syntax-rules ()
           ((_ def) (begin
                      (define x 7)
                      def))))
       (m (define y (lambda () x)))
       (define x 6)
       (y)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;   (()
   ;    (letrec ((x.0 (quote 5)))
   ;      ((case-lambda
   ;         (()
   ;          (letrec ((x.1 (quote 7)) (y.2 (case-lambda (() x.3))) (x.3 (quote 6)))
   ;            (y.2)))))))))
   ==> 6

   (let ()
     (define x 4)
     (define-syntax intro-ref
       (syntax-rules ()
         ((_ v) (define v x))))
     (let ()
       (define x 5)
       (intro-ref y)
       y))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;   (()
   ;    (letrec ((x.0 (quote 4)))
   ;      ((case-lambda (() (letrec ((x.1 (quote 5)) (y.2 x.0)) y.2))))))))
   ==> 4

   (let ((x 5))
     (let-syntax ((let-m (syntax-rules () ((_ m b)
                                           (let-syntax ((m (syntax-rules () ((_) x))))
                                             b)))))
       (let ((x 4))
         (let-m m (m)))))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;   ((x.0) ((case-lambda ((x.1) ((case-lambda (() x.0))))) (quote 4))))
   ; (quote 5))
   ==> 5

   (let ()
     (define-syntax define-and-ref-x
       (syntax-rules ()
         ((_ a) (begin (define a 5) x))))
     (define-and-ref-x x))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda (() (letrec ((x.0 5)) x.0))))
   ==>
   5

   (let ()
     (define-syntax def-m
       (syntax-rules ()
         ((_ m given-x)
          (begin (define x 1)
                 (define-syntax m
                   (syntax-rules ()
                     ((_) (begin (define given-x 2) x))))))))
     (def-m m x)
     (m))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda (() (letrec ((x.0 1) (x.1 2)) x.0))))
   ==>
   1

   (let ()
     (begin-meta
       (splicing-let ((x 5))
         (define foo (quote-syntax x))))
     (define-syntax (m stx)
       (syntax-case stx ()
         ((_) #`(let ((#,foo 7))
                  #,foo))))
     (splicing-let ((x 6))
       (m)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;   (()
   ;    (letrec ((x.0
   ;              (call-with-values
   ;               (lambda () ((quote #<procedure:values>)))
   ;               (case-lambda (_.1 (quote 6))))))
   ;      ((case-lambda ((x.2) x.2)) (quote 7))))))
   ==> 7

   (let ()
     (begin-meta
       (splicing-let ((x 5))
         (define foo (quote-syntax x))))
     (define-syntax (m stx)
       (syntax-case stx ()
         ((_ arg) #'(let ((arg 7))
                      arg))))
     (splicing-let ((x 6))
       (m foo)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (()
   ;   (letrec ((x.0
   ;             (call-with-values
   ;              (lambda () ((quote #<procedure:values>)))
   ;              (case-lambda (_.1 (quote 6))))))
   ;     ((case-lambda ((foo.2) foo.2)) (quote 7))))))
   ==> 7

   (let ()
     (begin-meta
       (splicing-let ((x 5))
         (define foo (quote-syntax x))))
     (define-syntax (m stx)
       (syntax-case stx ()
         ((_ arg) #'arg)))
     (splicing-let ((x 6))
       (m foo)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (()
   ;   (letrec ((x.0
   ;             (call-with-values
   ;              (lambda () ((quote #<procedure:values>)))
   ;              (case-lambda (_.1 (quote 6))))))
   ;     (quote x)))))
   ==> x

   ;; unbound identifier
   (let ()
     (begin-meta
      (splicing-let ((x 5))
        (define foo (quote-syntax x))))
     (define-syntax (m stx) foo)
     (splicing-let ((x 6))
       (m)))
   ==> error:parse

   (let ()
     (begin-meta
       (splicing-let ((x 5))
         (define foo (quote-syntax x))))
     (splicing-let ((x 6))
       (define-syntax (m stx) foo))
     (splicing-let ((x 7))
       (m)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (()
   ;   (letrec ((x.0
   ;             (call-with-values
   ;              (lambda () ((quote #<procedure:values>)))
   ;              (case-lambda (_.2 (quote 6)))))
   ;            (x.1 (quote 7)))
   ;     x.0))))
   ==> 6

   (let ()
     (splicing-let ((x 5))
       (define-in-vocabulary foo 'test (quote-syntax x)))
     (splicing-let ((x 6))
       (define-syntax (m stx)
         (lambda (env)
           (env-vocabulary-ref env (quote-syntax foo) 'test))))
     (splicing-let ((x 7))
       (m)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.1))))
   ==> 6

   (let ()
     (splicing-let ((x 5))
       (define-in-vocabulary foo
         'env (current-environment)
         'stx (quote-syntax x)))
     (splicing-let ((x 6))
       (define-in-vocabulary m
         'expression
         (lambda (env stx)
           (parse-expression
             (env-vocabulary-ref env (quote-syntax foo) 'env)
             (env-vocabulary-ref env (quote-syntax foo) 'stx)))))
     (splicing-let ((x 7))
       (m)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.0))))
   ==> 5

   (let ()
     (splicing-let ((x 5))
       (define-in-vocabulary foo
         'env (current-environment)
         'stx (quote-syntax x)))
     (splicing-let ((x 6))
       (define-in-vocabulary m
         'expression
         (lambda (env stx)
           (syntax-case-simple stx
             ((_ arg) (parse-expression
                        (env-vocabulary-ref env #'arg 'env)
                        (env-vocabulary-ref env #'arg 'stx)))))))
     (splicing-let ((x 7))
       (m foo)))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda
   ;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.0))))
   ==> 5

   (let ((x 88))
     (define-syntax-rule (m a)
       (begin (define a 77)
              x))
     (m x))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda ((x.0) (letrec ((x.1 (quote 77))) x.1))) (quote 88))
   ==> 77

   (let ((x 88))
     (define-syntax-rule (m a)
       (let ((a 77))
         x))
     (m x))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda ((x.0) ((case-lambda ((x.1) x.0)) (quote 77)))) (quote 88))
   ==> 88

   (let ()
     (define-syntax-rule (m a b)
       (begin (define a 1)
              b))
     (define-syntax-rule (n id)
       (m id x))
     (n x))
   ;EQUIVALENT RACKET CODE:
   ;((case-lambda (() (letrec ((x.0 (quote 1))) x.0))))
   ==> 1

   ;;; Different ways to intentionally choose the outermost binding of x to 6:
   (let ((x 6))
     (define-syntax (m2 stx)
       (quote-syntax x))
     (let ((x 5))
       (m2)))
   ==> 6
   (let ((x 6))
     (define-syntax (m stx)
       (define id1 (quote-syntax x))
       (quasiquote-syntax
         (begin
           (define-syntax (m3 stx)
             (quote-syntax x))
           (let ((#,id1 5))
             (m3)))))
     (m))
   ==> 6
   ;; These examples all require meta-level hygiene to behave properly.
   (let ((x 6))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax
           (quote-syntax x)))
       (define id1 (quote-syntax x))
       (define id2 (m2))
       (quasiquote-syntax
         (begin
           (define-syntax (m3 stx)
             (quote-syntax #,id2))
           (let ((#,id1 5))
             (m3)))))
     (m))
   ==> 6
   (let ((x 6))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax
           (quote-syntax x)))
       (define id1 (quote-syntax x))
       (define id2 (m2))
       (quasiquote-syntax
         (let ((#,id1 5))
           #,id2)))
     (m))
   ==> 5
   (let ((x 6))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax
           (quote-syntax x)))
       (define id1 (quote-syntax x))
       (define id2 (m2))
       (quasiquote-syntax
         (let ((#,id2 5))
           #,id1)))
     (m))
   ==> 5
   (let ((x 44))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax (quote-syntax x)))
       (quasiquote-syntax (let ((x 55)) #,(m2))))
     (m))
   ==> 55
   (let ((x 44))
     (define-syntax (m3 stx)
       (define-syntax (m4 stx)
         (quote-syntax (quote-syntax x)))
       (quasiquote-syntax (let ((#,(m4) 55)) x)))
     (m3))
   ==> 55
   (let ((x 44))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax
           (quote-syntax x)))
       (define id1 (quote-syntax x))
       (define id2 (m2))
       (quasiquote-syntax
         (let ()
           (define-syntax (m3 stx)
             (let ((#,id1 55))
               #,id2))
           (m3))))
     (m))
   ==> 55
   (let ((x 44))
     (define-syntax (m stx)
       (define-syntax (m2 stx)
         (quote-syntax
           (quote-syntax x)))
       (define id1 (quote-syntax x))
       (define id2 (m2))
       (quasiquote-syntax
         (let ()
           (define-syntax (m3 stx)
             (let ((#,id2 55))
               #,id1))
           (m3))))
     (m))
   ==> 55
   ))

(set! verbosity 0)
(test-evaluation
  env.test
 '(syntax-case
   (let ()
     (define-syntax (my-case stx)
       (syntax-case stx (else =>)
         ((_ e ((value ...) body ...) ... (else default))
          #'(let ((x e))
              (cond ((member x '(value ...)) body ...) ...
                    (else default))))
         ((_ e bad-clause ... (else default))
          (raise-parse-error "invalid my-case clause(s)" stx))
         ((_ e clause ... (=> default))
          #'(let ((x e)) (my-case x clause ... (else (default x)))))
         ((_ e clause ...)
          #'(let ((x e)) (my-case x clause ... (else (mistake "no matching case in my-case" x)))))))
     (list (my-case (+ 0 1)
                    ((0 1) 'zero-or-one)
                    ((2 3) 'two-or-three))
           (my-case (+ 1 1)
                    ((0 1) 'zero-or-one)
                    ((2 3) 'two-or-three))
           (my-case (+ 1 2)
                    ((0 1) 'zero-or-one)
                    ((2 3) 'two-or-three))
           (my-case (+ 2 2)
                    ((0 1) 'zero-or-one)
                    ((2 3) 'two-or-three)
                    (else  'other))
           (my-case (+ 2 2)
                    ((0 1) 'zero-or-one)
                    ((2 3) 'two-or-three)
                    (=> (lambda (n) `(other: ,n))))))
   ==> (zero-or-one two-or-three two-or-three other (other: 4))
   (let ()
     (define-syntax (my-case stx)
       (syntax-case stx (else =>)
         ((_ e ((value ...) body ...) ... (else default))
          #'(let ((x e))
              (cond ((member x '(value ...)) body ...) ...
                    (else default))))
         ((_ e bad-clause ... (else default))
          (raise-parse-error "invalid my-case clause(s)" stx))
         ((_ e clause ... (=> default))
          #'(let ((x e)) (my-case x clause ... (else (default x)))))
         ((_ e clause ...)
          #'(let ((x e)) (my-case x clause ... (else (mistake "no matching case in my-case" x)))))))
     (my-case (+ 2 2)
              ((0 1) 'zero-or-one)
              ((2 3) 'two-or-three)))
   ==> error:eval
   (let ()
     (define-syntax (my-case stx)
       (syntax-case stx (else =>)
         ((_ e ((value ...) body ...) ... (else default))
          #'(let ((x e))
              (cond ((member x '(value ...)) body ...) ...
                    (else default))))
         ((_ e bad-clause ... (else default))
          (raise-parse-error "invalid my-case clause(s)" stx))
         ((_ e clause ... (=> default))
          #'(let ((x e)) (my-case x clause ... (else (default x)))))
         ((_ e clause ...)
          #'(let ((x e)) (my-case x clause ... (else (mistake "no matching case in my-case" x)))))))
     (my-case (+ 2 2)
              ((0 1) 'zero-or-one)
              (else 'out-of-order)
              ((2 3) 'two-or-three)))
   ==> error:parse

   (let ()
     (define-syntax (my-let stx)
       (syntax-case stx ()
         ((_ ((x e) ...) . body)
          #'((lambda (x ...) . body) e ...))
         ((_ name ((x e) ...) . body)
          #'((letrec* ((name (lambda (x ...) . body))) name) e ...))))
     (list (my-let ((x 1) (y 2)) (+ x y))
           (my-let loop ((x* (range 11)))
                   (if (null? x*)
                       0
                       (+ (car x*) (loop (cdr x*)))))))
   ==> (3 55)

   (syntax->datum
     (syntax-case-simple #'()
       ((x ...) #'(got x ...))))
   ==> (got)
   (syntax->datum
     (syntax-case-simple #'(1 2 3)
       ((x ...) #'(got x ...))))
   ==> (got 1 2 3)
   (syntax->datum
     (syntax-case-simple #'((1 2 3) (4 5 6))
       (((x ...) ...) #'(got x ... ...))))
   ==> (got 1 2 3 4 5 6)
   (syntax->datum
     (syntax-case-simple #'((1 2 3) (4 5 6))
       (((x ...) ...) #'(got (x ...) ...))))
   ==> (got (1 2 3) (4 5 6))
   (syntax->datum
     (syntax-case-simple #'(((a 1) (b 2) (c 3)) ((x 4) (y 5) (z 6)))
       ((((lhs rhs) ...) ...) #'(got (lhs ... ...) (rhs ... ...)))))
   ==> (got (a b c x y z) (1 2 3 4 5 6))
   (syntax->datum
     (syntax-case-simple #'(((a #t 1) (b #t 2) (c #t 3)) ((x #t 4) (y #t 5) (z #t 6)))
       ((((lhs #t rhs) ...) ...) #'(got (lhs ... ...) (rhs ... ...)))))
   ==> (got (a b c x y z) (1 2 3 4 5 6))
   (syntax->datum
     (syntax-case-simple #'(((a #t 1) (b #t 2) (c #t 3)) ((x #t 4) (y #t 5) (z #t 6)))
       ((((lhs #f rhs) ...) ...) #'(false (lhs ... ...) (rhs ... ...)))
       ((((lhs #t rhs) ...) ...) #'(true (lhs ... ...) (rhs ... ...)))))
   ==> (true (a b c x y z) (1 2 3 4 5 6))
   (syntax->datum
     (syntax-case-simple #'(((a #t 1) (b #t 2) (c #t 3) . 111) ((x #t 4) (y #t 5) (z #t 6) . 111) 222)
       ((((lhs #f rhs) ... . 111) ... 222) #'(false (lhs ... ...) (rhs ... ...)))
       ((((lhs #t rhs) ... . 111) ... 222) #'(true (lhs ... ...) (rhs ... ...)))))
   ==> (true (a b c x y z) (1 2 3 4 5 6))
   (syntax->datum
     (syntax-case-simple #'(((a #t 1) (b #t 2) (c #t 3) . 111) ((x #t 4) (y #t 5) (z #t 6) . 111) 222 . 333)
       ((((lhs #t rhs) ... . 111) ... 222)       #'(false (lhs ... ...) ((rhs ...) ...)))
       ((((lhs #t rhs) ... . 111) ... 222 . 333) #'(true  (lhs ... ...) ((rhs ...) ...)))))
   ==> (true (a b c x y z) ((1 2 3) (4 5 6)))
   (syntax->datum
     (syntax-case-simple #'(((a #t 1) (b #t 2) (c #t 3) . 111) ((x #t 4) (y #t 5) (z #t 6) . 111) 222 . 333)
       ((((lhs #t rhs) ... . 111) ... 222)       #'(false (lhs ... ...) (((rhs lhs) ...) ...)))
       ((((lhs #t rhs) ... . 111) ... 222 . 333) #'(true  (lhs ... ...) (((rhs lhs) ...) ...)))))
   ==> (true (a b c x y z) (((1 a) (2 b) (3 c)) ((4 x) (5 y) (6 z))))
   ))

(exit (if at-least-one-test-failed? 1 0))
