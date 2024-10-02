#lang racket/base
(require
  "../platform/racket/nscheme.rkt" "include.rkt"
  racket/include racket/match racket/pretty racket/splicing
  (prefix-in rkt: racket/base))
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define verbosity 0)
(define show-compiled-racket? (and #t (< 0 verbosity)))

(define env.test.tiny (env-conjoin/match (env-conjoin* env.minimal env.common)))

(struct error:parse (c) #:prefab)
(struct error:eval  (c) #:prefab)

(define (test-eval env stx)
  (define (work-safely work)
    (with-escape
     (lambda x* (rkt:raise (apply vector 'panic x*)))
     (lambda (escape) (current-panic-handler escape work))))
  (when (< 0 verbosity)
    (displayln "EXPRESSION:")
    (pretty-write stx))
  (with-handlers ((vector? (lambda (c)
                             (when (< 0 verbosity)
                               (displayln "PARSE ERROR:")
                               (pretty-write c))
                             (error:parse c))))
    (let ((E (work-safely (lambda () (parse-expression env stx)))))
      (when (< 2 verbosity)
        (displayln "PARSED:")
        (pretty-write E))
      (when (< 1 verbosity)
        (displayln "PRETTY PARSED:")
        (pretty-write (E-pretty E)))
      (when show-compiled-racket?
        (displayln "EQUIVALENT RACKET CODE:")
        (pretty-write (E-compile-rkt E '())))
      (with-handlers ((vector? (lambda (c)
                                 (when (< 0 verbosity)
                                   (displayln "EVALUATION ERROR:")
                                   (pretty-write c))
                                 (error:eval c))))
        (call-with-values
          (lambda () (work-safely (lambda () (E-eval E))))
          (case-lambda
            ((result) (when (< 0 verbosity)
                        (displayln "VALUE:")
                        (pretty-write result))
                      result)
            (result*  (when (< 0 verbosity)
                        (displayln "VALUES:")
                        (pretty-write result*))
                      `(values . ,result*))))))))

(define (display-border)
  (displayln "================================================================================"))

(define-syntax run-evaluation-tests-etc
  (syntax-rules (PAUSE! RESUME! STOP! ! ==>)
    ((_ env __ (test-successes test-failures))
     (let* ((tests-succeeded (length test-successes))
            (tests-failed    (length test-failures))
            (tests-total     (+ tests-succeeded tests-failed))
            (tests-passed    (- tests-total tests-failed)))
       (newline)
       (unless (= tests-passed tests-total)
         (display-border)
         (printf "Tests failed: ~a out of ~a\n" tests-failed tests-total)
         (display-border)
         (for-each (lambda (failure)
                     (match-define `(,actual ,expr ,type ,expected) failure)
                     (newline)
                     (pretty-write `(Expected: ,expr ,type ,expected))
                     (pretty-write `(Actual: ,actual)))
                   (reverse test-failures))
         (newline))
       (display-border)
       (printf "Tests passed: ~a out of ~a\n" tests-passed tests-total)
       (display-border)))
    ((_ env #t (test-successes test-failures) e.expr ==> e.expected tests ...)
     (begin
       (let ((expr 'e.expr) (expected 'e.expected))
         (when (< 0 verbosity) (newline))
         (when (< 2 verbosity) (pretty-write (list expr '==> expected)))
         (let ((actual (test-eval env expr)))
           (cond ((or (and (eq? expected 'error:parse) (error:parse? actual))
                      (and (eq? expected 'error:eval)  (error:eval?  actual))
                      (equal? expected actual))
                  (set! test-successes (cons `(,expr ==> ,actual) test-successes)))
                 (else (set! test-failures (cons `(,actual ,expr ==> ,expected) test-failures))
                       (unless (< 0 verbosity) (newline) (pretty-write (list expr '==> expected)))
                       (displayln "FAILED:")
                       (displayln "EXPECTED:")
                       (pretty-write expected)
                       (displayln "ACTUAL:")
                       (pretty-write actual)))))
       (run-evaluation-tests-etc env #t (test-successes test-failures) tests ...)))
    ((_ env #t history ! info tests ...)
     (begin (when (< 0 verbosity)
              (newline)
              (display-border)
              (pretty-write 'info)
              (display-border))
            (run-evaluation-tests-etc env #t history tests ...)))
    ((_ env running? history STOP! tests ...)
     (begin (newline)
            (display-border)
            (displayln "STOPPING EARLY!")
            (display-border)
            (run-evaluation-tests-etc env running? history)))
    ((_ env #t history PAUSE! tests ...)
     (begin (newline)
            (display-border)
            (displayln "PAUSING!")
            (display-border)
            (run-evaluation-tests-etc env #f history tests ...)))
    ((_ env #f history RESUME! tests ...)
     (begin (newline)
            (display-border)
            (displayln "RESUMING!")
            (display-border)
            (run-evaluation-tests-etc env #t history tests ...)))
    ((_ env #t history RESUME! tests ...) (run-evaluation-tests-etc env #t history tests ...))
    ((_ env #f history PAUSE!  tests ...) (run-evaluation-tests-etc env #f history tests ...))
    ((_ env #f history a ==> b tests ...) (run-evaluation-tests-etc env #f history tests ...))
    ((_ env #f history ! info  tests ...) (run-evaluation-tests-etc env #f history tests ...))))

(define-syntax-rule (run-evaluation-tests env tests ...)
  (let ((test-successes '()) (test-failures '()))
    (run-evaluation-tests-etc env #t (test-successes test-failures) tests ...)))

(run-evaluation-tests
  env.test.tiny
  ! constants
  5         ==> 5
  '5        ==> 5
  'foo      ==> foo
  #t        ==> #t
  '()       ==> ()
  '#()      ==> #()
  '#(1 2 3) ==> #(1 2 3)
  '(1 2 3)  ==> (1 2 3)

  ! unbound-variable
  xyz            ==> error:parse
  (lambda (x) y) ==> error:parse

  ! primitives
  (cons 1 2)                    ==> (1 . 2)
  (car (cons 1 2))              ==> 1
  (cdr (cons 1 2))              ==> 2
  (pair? '(x x))                ==> #t
  (pair? #t)                    ==> #f
  (pair? (lambda x x))          ==> #f
  (procedure? '(x x))           ==> #f
  (procedure? '#t)              ==> #f
  (procedure? (lambda x x))     ==> #t
  (vector)                      ==> #()
  (vector 3 1)                  ==> #(3 1)
  (vector-length (vector))      ==> 0
  (vector-length (vector 3 1))  ==> 2
  (vector-ref (vector 5) 0)     ==> 5
  (vector-ref (vector 3 1 2) 0) ==> 3
  (vector-ref (vector 3 1 2) 1) ==> 1
  (vector-ref (vector 3 1 2) 2) ==> 2
  (pair? (vector 3 1 2))        ==> #f
  (procedure? (vector 3 1 2))   ==> #f
  (vector? (vector))            ==> #t
  (vector? (vector 3 1))        ==> #t
  (vector? '(x x))              ==> #f
  (vector? (lambda x x))        ==> #f
  (vector-ref '#(1 2 3) 0)      ==> 1
  (vector-ref '#(4 5 6) 2)      ==> 6
  (vector-ref '#(7 8 9) 1)      ==> 8
  (vector-ref
    (car (cdr (cons 6 (cons (vector 7 (cons 8 9) 0)
                            (cons 1 '())))))
    1)
  ==> (8 . 9)
  (vector-ref
    (car (cdr (cons 6 (cons (vector 7 (cons 8 9) 0 (car '(5 . #f)))
                            (cons 1 '())))))
    3)
  ==> 5

  ! assert
  (assert (< 1 2) (eqv? 2 2)) ==> (values)
  (assert (< 1 2) (eqv? 1 2)) ==> error:eval

  ! if
  (if #t 'yes 'no)               ==> yes
  (if #f 'yes 'no)               ==> no
  (if 0 'yes 'no)                ==> yes
  (if (car '(#t . #f)) 'yes 'no) ==> yes
  (if (cdr '(#t . #f)) 'yes 'no) ==> no

  ! and
  (and)        ==> #t
  (and 1)      ==> 1
  (and #f 2)   ==> #f
  (and 2 3)    ==> 3
  (and 2 3 4)  ==> 4
  (and 2 #f 4) ==> #f

  ! or
  (or)         ==> #f
  (or 1)       ==> 1
  (or #f 2)    ==> 2
  (or 2 3)     ==> 2
  (or #f #f 4) ==> 4
  (or 2 #f 4)  ==> 2

  ! when
  (when 1 2)                         ==> 2
  (let-values ((v* (when #f 3))) v*) ==> ()

  ! unless
  (let-values ((v* (unless 1 2))) v*) ==> ()
  (unless #f 3)                       ==> 3

  ! case-values
  (case-values (values 1 2 3)
    (()         'nothing)
    ((a)        (vector 'one a))
    ((a b)      (vector 'two a b))
    ((a b c)    (vector 'three a b c))
    ((a b c d)  (vector 'four a b c d))
    ((a b . x*) (vector '>=2 a b x*)))
  ==> #(three 1 2 3)
  (case-values (values 1 2 3)
    (()         'nothing)
    ((a)        (vector 'one a))
    ((a b)      (vector 'two a b))
    ((a b . x*) (vector '>=2 a b x*))
    ((a b c)    (vector 'three a b c))
    ((a b c d)  (vector 'four a b c d)))
  ==> #(>=2 1 2 (3))
  (case-values (values 1 2 3)
    (()         'nothing)
    ((a)        (vector 'one a))
    ((a b)      (vector 'two a b))
    ((a b c d)  (vector 'four a b c d)))
  ==> error:eval

  ! cond
  (cond (1 2))        ==> 2
  (cond (#f 3) (4 5)) ==> 5
  (cond ((<    2 1) '(< 2 1))
        ((eqv? 2 1) '(eqv? 2 1))
        (else       'else))
  ==> else
  (cond ((<    2 1) '(< 2 1))
        ((eqv? 2 1) '(eqv? 2 1))
        (else       'else)
        (#t         'unreachable))
  ==> error:parse
  (cond ((<    2 1) '(< 2 1))
        ((eqv? 2 1) '(eqv? 2 1))
        (7          => (lambda (x) (+ x 1)))
        (else       'else))
  ==> 8

  ! case
  (case 5
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (else    'd))
  ==> b
  (case 15
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (else    'd))
  ==> d
  (case 15
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (=> (lambda (x) (+ x 1))))
  ==> 16

  ! case1
  (case1 5
    (2    'a)
    (5    'b)
    (8    'c)
    (else 'd))
  ==> b
  (case1 15
    (2    'a)
    (5    'b)
    (8    'c)
    (else 'd))
  ==> d
  (case1 15
    (2 'a)
    (5 'b)
    (8 'c)
    (=> (lambda (x) (+ x 1))))
  ==> 16

  ! lambda
  ((lambda (x y) x) 5 6)                             ==> 5
  ((lambda (x y) y) 5 6)                             ==> 6
  ((lambda (x y) (cons y x)) 5 6)                    ==> (6 . 5)
  ((lambda (a b c) (cons b c)) '() 1 '(2))           ==> (1 2)
  ((lambda (a b . c) (cons b c)) '() 1 2 3)          ==> (1 2 3)
  ((lambda x x) 4 5 6)                               ==> (4 5 6)
  ((lambda (x) (cons (cdr x) (car x))) (cons #t #f)) ==> (#f . #t)
  ((lambda (w _ x __ y . z)
     (if (x y z '(a ... z))
         'true
         'false))
   1 2 (lambda x x) 4 5 6 7)
  ==> true
  ((lambda (w _ x __ y . z)
     (if (x y z '(a ... z))
         'true
         'false))
   1 2 (lambda x #f) 4 5 6 7)
  ==> false
  ((lambda (w _ x __ y . z)
     (if 'true
         (x y z '(a ... z))
         'false))
   1 2 (lambda x x) 4 5 6 7)
  ==> (5 (6 7) (a ... z))

  ! shadowing
  ((lambda lambda lambda) 'ok) ==> (ok)

  ! let
  (let ((x 8)) x)                             ==> 8
  (let ((x 9)) (let ((x 20)) x))              ==> 20
  (let ((x 9)) (let ((y 20)) x))              ==> 9
  (let ((x 10) (z 4)) (let ((x 11) (y x)) z)) ==> 4
  (let ((x 10) (y 4)) (let ((x 11) (y x)) y)) ==> 10
  (let ((p (cons 1 2)))
    (cons (cdr p) (cons (car p) '())))
  ==> (2 1)
  (let ((p (cons 1 2)))
    ((lambda x x) (cdr p) (car p)))
  ==> (2 1)
  (let ((op    (lambda (x) (car x)))
        (datum '(#t . #f))
        (ta    'yes)
        (fa    'no))
    (if (op datum) ta fa))
  ==> yes
  (let ((op    (lambda (x) (cdr x)))
        (datum '(#t . #f))
        (ta    'yes)
        (fa    'no))
    (if (op datum) ta fa))
  ==> no
  (let loop ((xs '(a b c)) (acc '()))
    (if (null? xs)
        acc
        (loop (cdr xs) (cons (car xs) acc))))
  ==> (c b a)

  (let ((list (lambda x x)))
    (list
      (list)
      (list 6 7)
      (pair? (list))
      (pair? (list 6 7))
      (pair? '(6 . 7))))
  ==> (() (6 7) #f #t #t)

  ! let*
  (let* ((a 1) (b (cons 2 a))) b) ==> (2 . 1)
  (let* ((a 1) (a (cons 2 a))) a) ==> (2 . 1)

  ! internal-define
  ((lambda (a b c)
     (define x (lambda () (+ (+ c y) z)))
     (define y b)
     (define z a)
     (x))
   1 2 3)
  ==> 6
  ((lambda (a b c)
     (begin (define x (lambda () (+ (+ c y) z)))
            (define y b))
     (define z a)
     (x))
   1 2 3)
  ==> 6
  ((lambda (a b c)
     (define list (lambda x x))
     (define ((f w) x y) (list w x y))
     ((f a) b c))
   1 2 3)
  ==> (1 2 3)
  (let ((list (lambda x x)))
    ((lambda (a b c)
       (define ((f w) x y) (list w x y))
       ((f a) b c))
     1 2 3))
  ==> (1 2 3)
  (let ((x 1) (y 7) (z 33))
    (define u 88)
    (define z y)
    6
    (begin (define a 5) (define w 4))
    z)
  ==> 7
  (let ((x 1) (y 7) (z 33))
    (define y 88)
    (define z y)
    6
    (begin (define a 5) (define w 4))
    z)
  ==> 88
  (let ((x 1) (y 7) (z 33))
    (define y 88)
    (define z y)
    6
    (begin (define a 5) (define w 4))
    a)
  ==> 5
  (let ((x 1) (y 7) (z 33))
    (define y 88)
    (define z (lambda (x) x))
    6
    (begin (define a 5) (define w (z y)))
    w)
  ==> 88
  (let ())              ==> error:parse
  (let () (begin))      ==> error:parse
  (let () (define x 5)) ==> error:parse

  ! internal-define-values
  (let ((a 5) (b 6))
    (define-values (a b c) (values 1 2 3))
    ((lambda x* x*) c b a))
  ==> (3 2 1)
  (let ((a 5) (b 6))
    (define vec.value* 'irrelevant)
    (define-values (a b c) (values 1 2 3))
    ((lambda x* x*) c b a))
  ==> (3 2 1)
  (let ((a 5) (b 6))
    (define-values (a b c) (values 1 2 3))
    (define vec.value* 'irrelevant)
    ((lambda x* x*) c b a))
  ==> (3 2 1)

  ;; Ignoring a binding by using #f instead of an identifier is currently not supported.
  ;! ignored-bindings
  ;((lambda (x y . #f) y) 10 11 12)   ==> 11
  ;((lambda (x #f y) x) 10 11 12)     ==> 10
  ;(let    ((x 15) (#f 2) (y 16)) y)  ==> 16
  ;(letrec ((x 14) (#f 2) (y 15)) x)  ==> 14
  ;(let ()
  ;  (define x  20)
  ;  (define #f 21)
  ;  (define y  22)
  ;  x)
  ;==> 20

  ! forced-expression
  (let ((x (expression 5)))
    x)
  ==> 5
  (let ()
    (define x 5)
    (expression x))
  ==> 5
  (let-values ((x* (expression (values 5 6))))
    x*)
  ==> (5 6)
  (let ()
    (define x 5)
    (expression (values x 6)))
  ==> (values 5 6)
  (let ()
    (expression (define x 5))
    (expression x))
  ==> error:parse

  ! letrec*
  (letrec* ((w (lambda () (y)))
            (x 32)
            (y (lambda () x)))
    (w))
  ==> 32
  (letrec* ((w (lambda () y))
            (x 33)
            (y x))
    (w))
  ==> 33

  ! begin
  (begin 1)   ==> 1
  (begin 1 2) ==> 2
  (let ((x 1))
    (let ((y (begin x x)))
      y))
  ==> 1

  ! splicing
  (let ()
    (splicing-let
      ((y 1))
      y))
  ==> 1
  (let ()
    (splicing-let
      ((y 2))
      (define z y)
      z))
  ==> 2
  (let ()
    (splicing-local
      ((define y 3))
      (define z y)
      z))
  ==> 3
  (let ((x (splicing-let
             ((y 4))
             4)))
    x)
  ==> 4
  (let ((x (begin
             (splicing-let
               ((y 44))
               44))))
    x)
  ==> 44
  (let ((x (splicing-local
             ((define y 5))
             y)))
    x)
  ==> 5
  (let ((x (begin
             (splicing-local
               ((define y 55))
               y))))
    x)
  ==> 55
  (let ((x (begin
             (splicing-let
               ((y 6))
               (define z y)
               z))))
    x)
  ==> error:parse
  (let ((x (begin
             (splicing-local
               ((define y 7))
               y))))
    x)
  ==> 7
  (let ((x (begin
             (splicing-local
               ((define y 7))
               (define z y)
               z))))
    x)
  ==> error:parse

  ! fixed-points
  (let ((list (lambda xs xs))
        (fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda (a b) ((x x) a b))))))))
    (let ((append
            (fix (lambda (append)
                   (lambda (xs ys)
                     (if (null? xs)
                         ys
                         (cons (car xs) (append (cdr xs) ys))))))))
      (list (append '() '())
            (append '(foo) '(bar))
            (append '(1 2) '(3 4)))))
  ==> (() (foo bar) (1 2 3 4))
  (let ((list (lambda xs xs))
        (fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda a (apply (x x) a))))))))
    (let ((append
            (fix (lambda (append)
                   (lambda (xs ys)
                     (if (null? xs)
                         ys
                         (cons (car xs) (append (cdr xs) ys))))))))
      (list (append '() '())
            (append '(foo) '(bar))
            (append '(1 2) '(3 4)))))
  ==> (() (foo bar) (1 2 3 4))
  (let ((list (lambda xs xs))
        (fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda a (apply (x x) a))))))))
    (let ((map (fix (lambda (map)
                      (lambda (f xs)
                        (if (null? xs)
                            '()
                            (cons (f (car xs)) (map f (cdr xs)))))))))
      (let ((fix*
              (fix (lambda (fix*)
                     (lambda fs
                       (map (lambda (fi)
                              (lambda a
                                (apply (apply fi (apply fix* fs)) a)))
                            fs))))))
        (let ((even&odd
                (fix* (lambda (even? odd?)
                        (lambda (n) (if (null? n)
                                        #t
                                        (odd? (cdr n)))))
                      (lambda (even? odd?)
                        (lambda (n)
                          (if (null? n)
                              #f
                              (even? (cdr n))))))))
          (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
            (list (even? '())    (odd? '())
                  (even? '(s))   (odd? '(s))
                  (even? '(s s)) (odd? '(s s))))))))
  ==> (#t #f #f #t #t #f)

  ! set!
  (let ()
    (mdefine x 100)
    x)
  ==> 100
  (let ()
    (mdefine x 100)
    (set! x (+ x 1))
    x)
  ==> 101
  (mlet ((x 200))
    x)
  ==> 200
  (mlet ((x 200))
    (set! x (+ x 1))
    x)
  ==> 201

  ! quasiquote
  `11                                 ==> 11
  `(1 2 3)                            ==> (1 2 3)
  `,(+ 1 2)                           ==> 3
  `(4 ,(+ 2 3) ,'6)                   ==> (4 5 6)
  `#(1 2 3)                           ==> #(1 2 3)
  `#(4 ,(+ 2 3) ,'6)                  ==> #(4 5 6)
  `(7 ,@(cons 8 (cons 9 '())) 10)     ==> (7 8 9 10)
  `#(7 ,@(cons 8 (cons 9 '())) 10)    ==> #(7 8 9 10)
  ``11                                ==> `11
  ``(1 2 3)                           ==> `(1 2 3)
  ``,(+ 1 2)                          ==> `,(+ 1 2)
  ``,,(+ 1 2)                         ==> `,3
  ``(4 ,(+ 2 3) ,'6)                  ==> `(4 ,(+ 2 3) ,'6)
  ``(4 ,,(+ 2 3) ,,'6)                ==> `(4 ,5 ,6)
  `(4 `,,(+ 2 3) ,(+ 3 3))            ==> (4 `,5 6)
  ``#(1 2 3)                          ==> `#(1 2 3)
  ``#(4 ,(+ 2 3) ,'6)                 ==> `#(4 ,(+ 2 3) ,'6)
  ``#(4 ,,(+ 2 3) ,,'6)               ==> `#(4 ,5 ,6)
  `#(4 `,,(+ 2 3) ,(+ 3 3))           ==> #(4 `,5 6)
  ``(7 ,@(cons 8 (cons 9 '())) 10)    ==> `(7 ,@(cons 8 (cons 9 '())) 10)
  ``(7 ,@',(cons 8 (cons 9 '())) 10)  ==> `(7 ,@'(8 9) 10)
  ``#(7 ,@(cons 8 (cons 9 '())) 10)   ==> `#(7 ,@(cons 8 (cons 9 '())) 10)
  ``#(7 ,@',(cons 8 (cons 9 '())) 10) ==> `#(7 ,@'(8 9) 10)

  ! match
  (match 5 (6 'six)) ==> error:eval
  (match 5
    (6 'six)
    (5 'five)
    (x (cons 'x: (cons x '())))
    (_ 'something-else))
  ==> five
  (match 7
    (6 'six)
    (5 'five)
    (x (cons 'x: (cons x '())))
    (_ 'something-else))
  ==> (x: 7)
  (match 7
    (6 'six)
    (5 'five)
    (_ 'something-else))
  ==> something-else
  (match '(1 2 3)
    (6        'six)
    (5        'five)
    ('(1 2 3) 'one-two-three)
    (x        (cons 'x: (cons x '())))
    (_        'something-else))
  ==> one-two-three
  (match '(1 2 3)
    (6               'six)
    (5               'five)
    ((cons x '(2 3)) (cons 'first: (cons x '())))
    ('(1 2 3)        'one-two-three)
    (x               (cons 'x: (cons x '())))
    (_               'something-else))
  ==> (first: 1)
  (match '(1 2 3)
    (6                      'six)
    (5                      'five)
    ((cons 1 (cons x '(3))) (cons 'second: (cons x '())))
    ('(1 2 3)               'one-two-three)
    (x                      (cons 'x: (cons x '())))
    (_                      'something-else))
  ==> (second: 2)
  (match '(1 2 3)
    (6            'six)
    (5            'five)
    ((list 1 x 3) (cons 'second: (cons x '())))
    ('(1 2 3)     'one-two-three)
    (x            (cons 'x: (cons x '())))
    (_            'something-else))
  ==> (second: 2)
  (match '(1 2 3)
    (6                'six)
    (5                'five)
    ((cons* 1 x '(3)) (cons 'second: (cons x '())))
    ('(1 2 3)         'one-two-three)
    (x                (cons 'x: (cons x '())))
    (_                'something-else))
  ==> (second: 2)
  (match '#(1 2 3)
    (6                      'six)
    (5                      'five)
    ((cons 1 (cons x '(3))) (cons 'second: (cons x '())))
    ('(1 2 3)               'one-two-three)
    (x                      (cons 'x: (cons x '())))
    (_                      'something-else))
  ==> (x: #(1 2 3))
  (match '#(1 2 3)
    (6                      'six)
    (5                      'five)
    ((cons 1 (cons x '(3))) (cons 'second: (cons x '())))
    ('#(1 2 3)              'one-two-three)
    (x                      (cons 'x: (cons x '())))
    (_                      'something-else))
  ==> one-two-three
  (match '#(1 2 3)
    (6              'six)
    (5              'five)
    ((vector 1 x 3) (cons 'second: (cons x '())))
    ('#(1 2 3)      'one-two-three)
    (x              (cons 'x: (cons x '())))
    (_              'something-else))
  ==> (second: 2)
  (match '#(1 2 3)
    (6              'six)
    (5              'five)
    ((? vector?)    'vector)
    ((vector 1 x 3) (cons 'second: (cons x '())))
    ('#(1 2 3)      'one-two-three)
    (x              (cons 'x: (cons x '())))
    (_              'something-else))
  ==> vector
  (match 'sym
    (6                   'six)
    (5                   'five)
    ((and (? symbol?) x) (cons 'symbol: (cons x '())))
    ((vector 1 x 3)      (cons 'second: (cons x '())))
    ('#(1 2 3)           'one-two-three)
    (x                   (cons 'x: (cons x '())))
    (_                   'something-else))
  ==> (symbol: sym)
  (match '(1 . 2)
    ((cons _ _) 'pair)
    (_          'something-else))
  ==> pair
  (match '(1 2 3)
    (5         'five)
    (`(1 ,x 3) (cons 'second: (cons x '())))
    (_         'something-else))
  ==> (second: 2)
  (match '#(1 2 3)
    (5          'five)
    (`(1 ,x 3)  (cons 'second: (cons x '())))
    (`#(1 ,x 3) (cons 'vector-second: (cons x '())))
    (_          'something-else))
  ==> (vector-second: 2)
  (match '#(1 2 3)
    (5           'five)
    (``(1 ,x 3)  'second)
    (``#(1 ,x 3) 'vector-second)
    (_           'something-else))
  ==> something-else
  (match '#(1 2 3)
    (5            'five)
    (``(1 ,,x 3)  (cons 'second: (cons x '())))
    (``#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (_            'something-else))
  ==> something-else
  (match '(1 2 3)
    (5            'five)
    (``(1 ,,x 3)  (cons 'second: (cons x '())))
    (``#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (_            'something-else))
  ==> something-else
  (match '(quasiquote (1 (unquote 2) 3))
    (5            'five)
    (``(1 ,,x 3)  (cons 'second: (cons x '())))
    (``#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (_            'something-else))
  ==> (second: 2)
  (match '(quasiquote #(1 (unquote 2) 3))
    (5            'five)
    (``(1 ,,x 3)  (cons 'second: (cons x '())))
    (``#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (_            'something-else))
  ==> (vector-second: 2)
  (match '(1 1 1)
    ((list x 1 y) (guard (eqv? x y)) 'equal)
    (_            'something-else))
  ==> equal
  (match '(1 1 1)
    ((list x 1 y) (guard (eqv? x y) (integer? x)) 'equal-int)
    (_            'something-else))
  ==> equal-int
  (match '(x 1 x)
    ((list x 1 y) (guard (eqv? x y)) 'equal)
    (_            'something-else))
  ==> equal
  (match '(x 1 x)
    ((list x 1 y) (guard (eqv? x y) (integer? x)) 'equal-int)
    (_            'something-else))
  ==> something-else
  (match '(1 1 1)
    ((list x 1 (not (? symbol?))) 'not-symbol)
    (_                            'something-else))
  ==> not-symbol
  (match '(1 1 q)
    ((list x 1 (not (? symbol?))) 'not-symbol)
    (_                            'something-else))
  ==> something-else
  (match '(1 1 q)
    ((list x 1 (or (? symbol?) (? boolean?))) 'symbol-or-boolean)
    (_                                        'something-else))
  ==> symbol-or-boolean
  (match '(1 1 #f)
    ((list x 1 (or (? symbol?) (? boolean?))) 'symbol-or-boolean)
    (_                                        'something-else))
  ==> symbol-or-boolean
  (match '(1 1 1)
    ((list x 1 (or (? symbol?) (? boolean?))) 'symbol-or-boolean)
    (_                                        'something-else))
  ==> something-else
  (match '(1 1 q)
    ((list x 1 (or (? symbol? y) (? boolean? y))) (cons 'symbol-or-boolean y))
    (_                                            'something-else))
  ==> (symbol-or-boolean . q)
  (match '(1 1 #f)
    ((list x 1 (or (? symbol? y) (? boolean? y))) (cons 'symbol-or-boolean y))
    (_                                            'something-else))
  ==> (symbol-or-boolean . #f)
  (match '(1 1 q)
    ((list x 1 (or (? symbol? y) (? boolean? z))) (cons 'symbol-or-boolean y))
    (_                                            'something-else))
  ==> error:parse
  (match '(1 1 #f)
    ((list x 1 (not (or (? symbol? y) (? boolean? y)))) (cons 'symbol-or-boolean y))
    (_                                                  'something-else))
  ==> error:parse
  (match '(1 1 q)
    ((list (? (lambda (n) (< 1 n)) x) 1 y)
     (cons 'large-enough x))
    (_ 'something-else))
  ==> something-else
  (match '(1 1 q)
    ((list (app (lambda (n) (+ n 1)) (? (lambda (n) (< 1 n)) x)) 1 y)
     (cons 'large-enough x))
    (_ 'something-else))
  ==> (large-enough . 2)
  (match '(1 2 3 4 5 6)
    ((list x y* ... z w)
     (vector 'ellipsis-matched x y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (2 3 4) 5 6)
  (match '(1 ((a 2) (b 3) (c 4)) 5 6)
    ((list x `((,q* ,y*) ...) z w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c) (2 3 4) 5 6)
  (match '(1 ((a 2 22 222) (b 3 33) (c) (d 4)) 5 6)
    ((list x `((,q* ,y* ...) ...) z w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c d) ((2 22 222) (3 33) () (4)) 5 6)
  (match '(1 ((x a 2 22 222) (x b 3 33) (x c) (x d 4)) 5 6)
    (`(,x ((x ,q* ,y* ...) ...) ,z ,w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c d) ((2 22 222) (3 33) () (4)) 5 6)
  (match '#(1 2 3 4 5 6)
    ((vector v y* ... z w)
     (vector 'ellipsis-matched v y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (2 3 4) 5 6)
  (match '#(1 #(#(a 2) #(b 3) #(c 4)) 5 6)
    ((vector x `#(#(,q* ,y*) ...) z w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c) (2 3 4) 5 6)
  (match '#(1 #(#(a 2 22 222) #(b 3 33) #(c) #(d 4)) 5 6)
    ((vector x `#(#(,q* ,y* ...) ...) z w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c d) ((2 22 222) (3 33) () (4)) 5 6)
  (match '#(1 #(#(x a 2 22 222) #(x b 3 33) #(x c) #(x d 4)) 5 6)
    (`#(,x #(#(x ,q* ,y* ...) ...) ,z ,w)
     (vector 'ellipsis-matched x q* y* z w))
    (_ 'something-else))
  ==> #(ellipsis-matched 1 (a b c d) ((2 22 222) (3 33) () (4)) 5 6)

  ! qmatch
  (qmatch 5 (6 'six)) ==> error:eval
  (qmatch 5
    (6  'six)
    (5  'five)
    (,x (cons 'x: (cons x '())))
    (,_ 'something-else))
  ==> five
  (qmatch 7
    (6  'six)
    (5  'five)
    (,x (cons 'x: (cons x '())))
    (,_ 'something-else))
  ==> (x: 7)
  (qmatch 7
    (6  'six)
    (5  'five)
    (,_ 'something-else))
  ==> something-else
  (qmatch '(1 2 3)
    (6       'six)
    (5       'five)
    ((1 2 3) 'one-two-three)
    (,x      (cons 'x: (cons x '())))
    (,_      'something-else))
  ==> one-two-three
  (qmatch '(1 2 3)
    (6            'six)
    (5            'five)
    ((,x . (2 3)) (cons 'first: (cons x '())))
    ((1 2 3)      'one-two-three)
    (,x           (cons 'x: (cons x '())))
    (,_           'something-else))
  ==> (first: 1)
  (qmatch '(1 2 3)
    (6        'six)
    (5        'five)
    ((1 ,x 3) (cons 'second: (cons x '())))
    ((1 2 3)  'one-two-three)
    (,x       (cons 'x: (cons x '())))
    (,_       'something-else))
  ==> (second: 2)
  (qmatch '#(1 2 3)
    (6        'six)
    (5        'five)
    ((1 ,x 3) (cons 'second: (cons x '())))
    ((1 2 3)  'one-two-three)
    (,x       (cons 'x: (cons x '())))
    (,_       'something-else))
  ==> (x: #(1 2 3))
  (qmatch '#(1 2 3)
    (6        'six)
    (5        'five)
    ((1 ,x 3) (cons 'second: (cons x '())))
    (#(1 2 3) 'one-two-three)
    (,x       (cons 'x: (cons x '())))
    (,_       'something-else))
  ==> one-two-three
  (qmatch '#(1 2 3)
    (6         'six)
    (5         'five)
    (#(1 ,x 3) (cons 'second: (cons x '())))
    (#(1 2 3)  'one-two-three)
    (,x        (cons 'x: (cons x '())))
    (,_        'something-else))
  ==> (second: 2)
  (qmatch '#(1 2 3)
    (6            'six)
    (5            'five)
    (,(? vector?) 'vector)
    (#(1 ,x 3)    (cons 'second: (cons x '())))
    (#(1 2 3)     'one-two-three)
    (,x           (cons 'x: (cons x '())))
    (,_           'something-else))
  ==> vector
  (qmatch 'sym
    (6                    'six)
    (5                    'five)
    (,(and (? symbol?) x) (cons 'symbol: (cons x '())))
    (#(1 ,x 3)            (cons 'second: (cons x '())))
    (#(1 2 3)             'one-two-three)
    (,x                   (cons 'x: (cons x '())))
    (,_                   'something-else))
  ==> (symbol: sym)
  (qmatch '(1 . 2)
    ((,_ . ,_) 'pair)
    (,_        'something-else))
  ==> pair
  (qmatch '(1 2 3)
    (5        'five)
    ((1 ,x 3) (cons 'second: (cons x '())))
    (,_       'something-else))
  ==> (second: 2)
  (qmatch '#(1 2 3)
    (5         'five)
    ((1 ,x 3)  (cons 'second: (cons x '())))
    (#(1 ,x 3) (cons 'vector-second: (cons x '())))
    (,_        'something-else))
  ==> (vector-second: 2)
  (qmatch '#(1 2 3)
    (5          'five)
    (`(1 ,x 3)  'second)
    (`#(1 ,x 3) 'vector-second)
    (,_         'something-else))
  ==> something-else
  (qmatch '#(1 2 3)
    (5           'five)
    (`(1 ,,x 3)  (cons 'second: (cons x '())))
    (`#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (,_          'something-else))
  ==> something-else
  (qmatch '(1 2 3)
    (5           'five)
    (`(1 ,,x 3)  (cons 'second: (cons x '())))
    (`#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (,_          'something-else))
  ==> something-else
  (qmatch '(quasiquote (1 (unquote 2) 3))
    (5           'five)
    (`(1 ,,x 3)  (cons 'second: (cons x '())))
    (`#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (,_          'something-else))
  ==> (second: 2)
  (qmatch '(quasiquote #(1 (unquote 2) 3))
    (5           'five)
    (`(1 ,,x 3)  (cons 'second: (cons x '())))
    (`#(1 ,,x 3) (cons 'vector-second: (cons x '())))
    (,_          'something-else))
  ==> (vector-second: 2)
  )

(define env.test.large
  (let ((env (make-env)))
    (env-bind! env 'env.test vocab.expression
               (parse/constant-expression ($quote env.large)))
    (env-conjoin env.large env)))

(run-evaluation-tests
  env.test.large

  ! bytevector-ports
  (call-with-output-bytevector
   (lambda (out)
     (oport-set-size! out 2)
     (call-with-input-bytevector
      #"testing 1 2 3"
      (lambda (in)
        (let ((buf (make-mbytevector 18 0)))
          (range-for-each (lambda (i) (mbytevector-set! buf i (+ 65 i))) (mbytevector-length buf))
          (iport-read in buf 5 6)
          (iport-read in buf 11 5)
          (oport-write out buf 0 3)
          (oport-write out buf 3 (- (mbytevector-length buf) 3)))))))
  ==>
  #"ABCDEtesting 1 2QR"
  (call-with-output-bytevector
   (lambda (out)
     (oport-set-size! out 2)
     (call-with-input-bytevector
      #"testing"
      (lambda (in)
        (let ((buf (make-mbytevector 20 0)))
          (range-for-each (lambda (i) (mbytevector-set! buf i (+ 65 i))) (mbytevector-length buf))
          (iport-read in buf 3 2)
          (iport-read in buf 5 (- (mbytevector-length buf) 5))
          (oport-write out buf 0 3)
          (oport-write out buf 3 (- (mbytevector-length buf) 3)))))))
  ==>
  #"ABCtestingKLMNOPQRST"
  (let ((buf (make-mbytevector 20 0)))
    (call-with-output-mbytevector
     buf
     (lambda (out)
       (let ((src #"testing 4 5 6 7 8 9 10 11"))
         (oport-write out src 0 8)
         (oport-write out src 10 (- (bytevector-length src) 10))
         (oport-flush out)
         (mbytevector->bytevector buf)))))
  ==>
  error:eval
  ;#(panic
  ;  raise
  ;  (#(error #(description) io-error #(tag detail))
  ;   .
  ;   #("oport-flush failed"
  ;     no-space
  ;     ((mbytevector-ostream 0 20) (write 0 23 23)))))
  (let ((buf (make-mbytevector 20 0)))
    (call-with-output-mbytevector
     buf
     (lambda (out)
       (let* ((src    #"testing 4 5 6 7 8 9 10 11")
              (amount (oport-write out src 0 8)))
         (oport-write out src 10 (min (- (bytevector-length src) 10)
                                      (- (mbytevector-length buf) amount)))
         (oport-flush out)
         (mbytevector->bytevector buf)))))
  ==>
  #"testing 5 6 7 8 9 10"

  ! string-writing
  (number->string -1)
  ==> "-1"
  (number->string 9)
  ==> "9"
  (number->string 10)
  ==> "10"
  (number->string 11)
  ==> "11"
  (number->string 19)
  ==> "19"
  (number->string 100)
  ==> "100"
  (number->string 123)
  ==> "123"
  (number->string -1)
  ==> "-1"
  (number->string -123)
  ==> "-123"
  (number->string 123/456)
  ==> "41/152"
  (number->string -123/456)
  ==> "-41/152"
  (number->string 1/2000  10 '((use-exponent-below . 1/10000)))
  ==>
  "0.0005"
  (number->string 1/20000 10 '((use-exponent-below . 1/10000)))
  ==>
  "5e-5"
  (number->string 1/30000 10 '((use-exponent-below . 1/10000)))
  ==>
  "3.~3e-5"
  (number->string 12 10 '((use-exponent-above . 100)))
  ==>
  "12"
  (number->string 12345 10 '((use-exponent-above . 100)))
  ==>
  "1.2345e4"
  (number->string 123456789123456789 10 '((use-exponent-above . 10000000000000)
                                          (use-exponent-below . 1/10000)))
  ==>
  "1.23456789123456789e17"
  (number->string 100/3 10 '((use-exponent-above . 10)))
  ==>
  "3.~3e1"
  (number->string 100/7 10 '((use-exponent-above . 10)))
  ==>
  "1.~428571e1"
  (number->string 1/43 10 '())
  ==>
  "0.~023255813953488372093"
  (number->string 1/47 10 '())
  ==>
  "0.~0212765957446808510638297872340425531914893617"
  (number->string 1/113 10 '())
  ==>
  "0.~0088495575221238938053097345132743362831858407079646017699115044247787610619469026548672566371681415929203539823"

  ! string-reading
  (string->number "0")
  ==> 0
  (string->number "9")
  ==> 9
  (string->number "10")
  ==> 10
  (string->number "11")
  ==> 11
  (string->number "19")
  ==> 19
  (string->number "100")
  ==> 100
  (string->number "123")
  ==> 123
  (string->number "-1")
  ==> -1
  (string->number "-123")
  ==> -123
  (string->number "123/456")
  ==> 123/456
  (string->number "-123/456")
  ==> -123/456
  (string->number "")
  ==> #f
  (string->number "123/")
  ==> #f
  (string->number "/456")
  ==> #f
  (string->number "-123/456.")
  ==> #f
  (string->number "1")
  ==> 1
  (string->number "1.")
  ==> 1
  (string->number ".1")
  ==> 1/10
  (string->number "1e2")
  ==> 100
  (string->number "3e-2")
  ==> 3/100
  (string->number "1.e3")
  ==> 1000
  (string->number ".1e5")
  ==> 10000
  (string->number "0.123")
  ==> 123/1000
  (string->number "10.123")
  ==> 10123/1000
  (string->number "-1")
  ==> -1
  (string->number "-1.")
  ==> -1
  (string->number "-.1")
  ==> -1/10
  (string->number "-1e2")
  ==> -100
  (string->number "-3e-2")
  ==> -3/100
  (string->number "-1.e3")
  ==> -1000
  (string->number "-.1e5")
  ==> -10000
  (string->number "-0.123")
  ==> -123/1000
  (string->number "-10.123")
  ==> -10123/1000
  (string->number "#b0")
  ==> 0
  (string->number "#b9")
  ==> #f
  (string->number "#b10")
  ==> 2
  (string->number "#b11")
  ==> 3
  (string->number "#b19")
  ==> #f
  (string->number "#b100")
  ==> 4
  (string->number "#b123")
  ==> #f
  (string->number "#x0")
  ==> 0
  (string->number "#x9")
  ==> 9
  (string->number "#x10")
  ==> 16
  (string->number "#x11")
  ==> 17
  (string->number "#x1f")
  ==> 31
  (string->number "#x100")
  ==> 256
  (string->number "1e#b10")
  ==> 4
  (string->number "3e#b-10")
  ==> 3/4
  (string->number "0.~3")
  ==> 1/3
  (string->number "0.~3333")
  ==> 1/3
  (string->number "0.~9")
  ==> 1
  (string->number "0.0~9")
  ==> 1/10
  (string->number "1.~9")
  ==> 2
  (string->number "1.~3")
  ==> 4/3
  (string->number "0.1~3")
  ==> 4/30

  ! generators
  (let ()
    (define (list . x*) x*)
    (define (g-range n)
      (make-generator
       'done
       (lambda (yield)
         (lambda (inc)
           (let loop ((i inc))
             (when (< i n)
               (loop (+ i (yield i)))))))))
    (define r10 (g-range 10))
    (list (r10 -1) (r10 0) (r10 1) (r10 3) (r10 3) (r10 3) (r10 3) (r10 3)))
  ==> (-1 -1 0 3 6 9 done done)
  (let ()
    (define (list . x*) x*)
    (define (g-range n)
      (make-generator
       'done
       (lambda (yield)
         (lambda (inc)
           (let loop ((i inc))
             (when (< i n)
               (loop (+ i (yield i)))))))))
    (define (indirect)
      (make-generator
       'forever
       (lambda (yield)
         (lambda (g)
           (let loop ()
             (yield (list (g 3) (g 3) (g 3)))
             (loop))))))
    (define r10 (g-range 10))
    (define ind (indirect))
    (list (ind r10) (ind r10)))
  ==> ((3 6 9) (done done done))

  ! coroutines
  (let ((ch.done (make-channel)))
    (define (list . x*) x*)
    (define (co-range n)
      (make-coroutine
       (lambda (inc)
         (let loop ((i inc))
           (when (< i n)
             (loop (+ i (main i)))))
         (let loop ()
           (main 'done)
           (loop)))))
    (define main (make-coroutine
                  (lambda (n)
                    (define r (co-range n))
                    (channel-put ch.done (list (r -1) (r 0) (r 1) (r 3) (r 3) (r 3) (r 3) (r 3))))))
    (thread (lambda () (main 10)))
    (channel-get ch.done))
  ==> (-1 -1 0 3 6 9 done done)
  (let ((ch.done (make-channel)))
    (define (list . x*) x*)
    (define (co-range n)
      (make-coroutine
        (lambda (inc)
          (let loop ((i inc))
            (when (< i n)
              (loop (+ i (yield i)))))
          (let loop ()
            (yield 'done)
            (loop)))))
    (define (indirect)
      (make-coroutine
        (lambda (g)
          (let loop ()
            (yield (list (g 3) (g 3) (g 3)))
            (loop)))))
    (define yield (make-coroutine
                   (lambda (n)
                     (define ind (indirect))
                     (define r10 (co-range n))
                     (channel-put ch.done (list (ind r10) (ind r10) (ind r10))))))
    (thread (lambda () (yield 10)))
    (channel-get ch.done))
  ==> (3 6 9)

  ! dynamic-parameters
  (let ((list    (lambda x* x*))
        (example (make-parameter 0)))
    (let* ((a (example))
           (b (example 'example-value example))
           (c (example)))
      (list a b c)))
  ==> (0 example-value 0)

  ! exception-handling
  (mlet ((missing '()) (mistake* '()) (restart-binding* '()))
    (with-raise-handler
     (lambda (exn)
       (set! restart-binding* (cons (current-restart*) restart-binding*))
       (cond
         ((unbound-identifier-parse-error? exn)
          (set! missing (cons (parse-error-syntax exn) missing)))
         ((parse-error? exn)
          (set! mistake* (cons (parse-error-syntax exn) mistake*))))
       (use-value ($quote `(REPLACED: ,(parse-error-syntax exn))))
       (continue))
     (lambda ()
       (list
        (parse-expression env.test '(foo 1 2))
        (parse-expression env.test '(foo bar baz))
        (with-continue-alternative*
         '(try the next alternative)
         (lambda () (parse-expression env.test '#(not-ok)))
         (lambda () (parse-expression env.test '#(also-not-ok)))
         (lambda () (parse-expression env.test '#(still-not-ok)))
         (lambda () 'final-alternative))
        (reverse missing)
        (reverse mistake*)
        (map (lambda (r*) (map (lambda (r) (list (restart-name r) (restart-description r))) r*))
             (reverse restart-binding*))))))
  ==>
  (#(E:call #(E:quote (REPLACED: foo)) (#(E:quote 1) #(E:quote 2)))
   #(E:call #(E:quote (REPLACED: foo)) (#(E:quote (REPLACED: bar)) #(E:quote (REPLACED: baz))))
   final-alternative
   (foo foo bar baz)
   (#(not-ok) #(also-not-ok) #(still-not-ok))
   (((continue (return unbound-variable-reference))
     (use-value (replace unbound-variable-reference)))
    ((continue (return unbound-variable-reference))
     (use-value (replace unbound-variable-reference)))
    ((continue (return unbound-variable-reference))
     (use-value (replace unbound-variable-reference)))
    ((continue (return unbound-variable-reference))
     (use-value (replace unbound-variable-reference)))
    ((continue (try the next alternative)))
    ((continue (try the next alternative)))
    ((continue (try the next alternative)))))
  (with-isolation
   (lambda x* `(got panic: . ,x*))
   (lambda () 'success))
  ==> success
  (with-isolation
   (lambda x* `(got panic: . ,x*))
   (lambda () (panic 'failure)))
  ==> (got panic: failure)
  (let ((ch (make-channel)))
    (isolated-thread
     (lambda x* (channel-put ch `(got panic: . ,x*)))
     (lambda () (channel-put ch 'success)))
    (channel-get ch))
  ==> success
  (let ((ch (make-channel)))
    (isolated-thread
     (lambda x* (channel-put ch `(got panic: . ,x*)))
     (lambda () (panic 'failure)))
    (channel-get ch))
  ==> (got panic: failure)
  )

(displayln "\nBeginning recursive panic test:")
(run-evaluation-tests
  env.test.large

  ! recursive-panic
  (current-panic-handler
   (lambda x* (begin (apply panic x*)))
   (lambda () (panic 'a 'b 'c)))
  ==> error:eval
)

(define env.test.posix env.large+posix+privileged)

#;(run-evaluation-tests
 env.test.posix

 ! cmdline
 command-line-argument* ==> #("bootstrap/test.rkt")

 ! stdio
 (begin
   (ostream-write-byte standard-output-stream 65)
   (ostream-write-byte standard-output-stream 66)
   (ostream-write-byte standard-output-stream 67)
   (ostream-write-byte standard-output-stream 10))
 ==> (values)
 (let* ((message #"Type 'x' and hit enter: ")
        (len (bytevector-length message)))
   (ostream-write standard-error-stream message 0 len len)
   (istream-read-byte standard-input-stream))
 ==> 120

 ! file-io
 (let* ((dir     "test-for-file-io-etc")
        (fname   (string-append dir "/out.txt"))
        (fname2  (string-append dir "/out2.txt"))
        (message #"Hello world!")
        (len     (bytevector-length message))
        (out     (begin
                   (case-values (make-directory/k dir values values)
                     (() (values))
                     ((tag d) (panic #f "make-directory failed" tag d)))
                   (case-values (open-file-ostream/k fname 'create values values)
                     ((out) out)
                     ((tag d) (panic #f "open-file-ostream failed" tag d))))))
   (case-values (ostream-write/k out message 0 len len values values)
     ((amount) (values))
     ((tag d)  (panic #f "ostream-write failed" tag d)))
   (ostream-close out)
   (let* ((size   (file-size/k fname values values))
          (dperm  (file-permissions/k dir values values))
          (fperm  (file-permissions/k fname values values))
          (dtype  (file-type/k dir values values))
          (ftype  (file-type/k fname values values))
          (path*  (directory-file*/k dir values values))
          (sec    (file-modified-seconds/k fname values values))
          (sec2   (case-values (move-file/k fname fname2 values values)
                    (() (file-modified-seconds/k fname2 values values))
                    ((tag d) (panic #f "move-file failed" tag d))))
          (path*2 (directory-file*/k dir values values))
          (in     (case-values (open-file-istream/k fname2 values values)
                    ((in) in)
                    ((tag d) (panic #f "open-file-istream failed" tag d))))
          (buf    (make-mbytevector size 0))
          (amount (case-values (istream-read/k in buf 0 size size values values values)
                    ((amount) amount)
                    ((tag d) (panic #f "istream-read failed" tag d)))))
     (istream-close in)
     (case-values (delete-file/k fname2 values values)
       (() (values))
       ((tag d) (panic #f "delete-file failed" tag d)))
     (case-values (delete-directory/k dir values values)
       (() (values))
       ((tag d) (panic #f "delete-directory failed" tag d)))
     (list size amount dperm fperm (eqv? sec sec2) dtype ftype path* path*2
           (mbytevector->bytevector buf))))
 ==> (12 12 #o755 #o644 #t directory file ("out.txt") ("out2.txt") #"Hello world!")

 (let* ((dir     "test-for-file-io-etc")
        (fname   (string-append dir "/out.txt"))
        (fname2  (string-append dir "/out2.txt"))
        (message #"Hello world!")
        (len     (bytevector-length message))
        (out     (begin (make-directory dir) (open-file-ostream fname 'create))))
   (ostream-write out message 0 len len)
   (ostream-close out)
   (let* ((size   (file-size fname))
          (dperm  (file-permissions dir))
          (fperm  (file-permissions fname))
          (dtype  (file-type dir))
          (ftype  (file-type fname))
          (path*  (directory-file* dir))
          (sec    (file-modified-seconds fname))
          (sec2   (begin (move-file fname fname2) (file-modified-seconds fname2)))
          (path*2 (directory-file* dir))
          (in     (open-file-istream fname2))
          (buf    (make-mbytevector size 0))
          (amount (istream-read in buf 0 size size)))
     (istream-close in)
     (delete-file fname2)
     (delete-directory dir)
     (list size amount dperm fperm (eqv? sec sec2) dtype ftype path* path*2
           (mbytevector->bytevector buf))))
 ==> (12 12 #o755 #o644 #t directory file ("out.txt") ("out2.txt") #"Hello world!")

 ! host-processes
 (call-with-output-bytevector
  (lambda (out)
    (let ((p (host-process #f #f "echo" '("hello world"))))
      (let loop ()
        (case-values (istream-read-byte (host-process-out p))
          (()  (oport-write-byte out (+ (host-process-exit-code p) 48)))
          ((b) (oport-write-byte out b) (loop)))))))
 ==>
 #"hello world\n0"

 (call-with-output-bytevector
  (lambda (out)
    (let ((p (host-process #f #f "cat" '())))
      (thread (lambda ()
                (call-with-input-bytevector
                 #"another example"
                 (lambda (in)
                   (let loop ()
                     (case-values (iport-read-byte in)
                       (()  (ostream-close (host-process-in p)))
                       ((b) (ostream-write-byte (host-process-in p) b) (loop))))))))
      (let loop ((sname 'out))
        (case-values (istream-read-byte
                      (if (eq? sname 'out) (host-process-out p) (host-process-err p)))
          (()  (if (eq? sname 'out)
                   (loop 'err)
                   (oport-write-byte out (+ (host-process-exit-code p) 48))))
          ((b) (oport-write-byte out b) (loop sname)))))))
 ==>
 #"another example0"
 )
