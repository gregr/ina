#lang racket/base
(require "../platform/racket/nscheme.rkt" racket/include racket/match racket/pretty racket/splicing)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(include "../include/boot/error.scm")
(include "../include/boot/record.scm")
(include "../include/boot/string.scm")
(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/base/misc.scm")
(include "../include/base/compare.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/string.scm")

(include "../include/syntax.scm")
(include "../include/ir.scm")
(include "../include/stage-simple.scm")
(include "../include/parse.scm")
(include "../include/primitive.scm")
(include "../include/minimal.scm")
(include "../include/match.scm")

;; TODO: all remaining compiler definitions should be included here, replacing eval-simple.scm:
(include "../include/eval-simple.scm")
;(include "../include/eval-simple-cps.scm")

;; TODO: the parsers defined here perform compile-time evaluation.  They should be adjusted to
;; depend on the compiler instead of E-eval:
(include "../include/extended.scm")

(define verbosity 0)

(define env.test (env-compose.match (env-compose env.primitive env.minimal)))

(struct error:parse (c) #:prefab)
(struct error:eval  (c) #:prefab)

(define (test-eval stx)
  (when (< 0 verbosity)
    (displayln "EXPRESSION:")
    (pretty-write stx))
  (with-handlers ((vector? (lambda (c)
                             (when (< 0 verbosity)
                               (displayln "PARSE ERROR:")
                               (pretty-write c))
                             (error:parse c))))
    (let ((E (parse-expression env.test stx)))
      (when (< 2 verbosity)
        (displayln "PARSED:")
        (pretty-write E))
      (when (< 1 verbosity)
        (displayln "PRETTY PARSED:")
        (pretty-write (E-pretty E)))
      (with-handlers ((vector? (lambda (c)
                                 (when (< 0 verbosity)
                                   (displayln "EVALUATION ERROR:")
                                   (pretty-write c))
                                 (error:eval c))))
        (call-with-values
          (lambda () (E-eval E))
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
    ((_ __ (test-successes test-failures))
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
    ((_ #t (test-successes test-failures) e.expr ==> e.expected tests ...)
     (begin
       (let ((expr 'e.expr) (expected 'e.expected))
         (when (< 0 verbosity) (newline))
         (when (< 2 verbosity) (printf "~s ==> ~s\n" expr expected))
         (let ((actual (test-eval expr)))
           (cond ((or (and (eq? expected 'error:parse) (error:parse? actual))
                      (and (eq? expected 'error:eval)  (error:eval?  actual))
                      (equal? expected actual))
                  (set! test-successes (cons `(,expr ==> ,actual) test-successes)))
                 (else (set! test-failures (cons `(,actual ,expr ==> ,expected) test-failures))
                       (unless (< 0 verbosity) (printf "\n~s ==> ~s\n" expr expected))
                       (displayln "FAILED:")
                       (displayln "EXPECTED:")
                       (pretty-write expected)
                       (displayln "ACTUAL:")
                       (pretty-write actual)))))
       (run-evaluation-tests-etc #t (test-successes test-failures) tests ...)))
    ((_ #t history ! info tests ...)
     (begin (when (< 0 verbosity)
              (newline)
              (display-border)
              (pretty-write 'info)
              (display-border))
            (run-evaluation-tests-etc #t history tests ...)))
    ((_ running? history STOP! tests ...)
     (begin (newline)
            (display-border)
            (displayln "STOPPING EARLY!")
            (display-border)
            (run-evaluation-tests-etc running? history)))
    ((_ #t history PAUSE! tests ...)
     (begin (newline)
            (display-border)
            (displayln "PAUSING!")
            (display-border)
            (run-evaluation-tests-etc #f history tests ...)))
    ((_ #f history RESUME! tests ...)
     (begin (newline)
            (display-border)
            (displayln "RESUMING!")
            (display-border)
            (run-evaluation-tests-etc #t history tests ...)))
    ((_ #t history RESUME! tests ...) (run-evaluation-tests-etc #t history tests ...))
    ((_ #f history PAUSE!  tests ...) (run-evaluation-tests-etc #f history tests ...))
    ((_ #f history a ==> b tests ...) (run-evaluation-tests-etc #f history tests ...))
    ((_ #f history ! info  tests ...) (run-evaluation-tests-etc #f history tests ...))))

(define-syntax-rule (run-evaluation-tests tests ...)
  (let ((test-successes '()) (test-failures '()))
    (run-evaluation-tests-etc #t (test-successes test-failures) tests ...)))

(run-evaluation-tests
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

  ! caseq
  (caseq 5
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (else    'd))
  ==> b
  (caseq 15
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (else    'd))
  ==> d
  (caseq 15
    ((1 2 3) 'a)
    ((4 5 6) 'b)
    ((7 8 9) 'c)
    (=> (lambda (x) (+ x 1))))
  ==> 16

  ! case1q
  (case1q 5
    (2    'a)
    (5    'b)
    (8    'c)
    (else 'd))
  ==> b
  (case1q 15
    (2    'a)
    (5    'b)
    (8    'c)
    (else 'd))
  ==> d
  (case1q 15
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
