#lang racket/base
(require
  "expand.rkt"
  "syntax.rkt"
  rackunit
  )

(define-syntax test
  (syntax-rules ()
    ((_ name actual expected)
     (begin (printf "testing: ~s\n" name)
            (check-equal? actual expected)))))

(define (ev stx) (evaluate #f env-initial (expand env-initial stx)))

(test 'literals
  (map ev (list #'(quote ()) #'#t #'4))
  '(() #t 4))

(test 'lambda-app-1
  (ev #'((lambda (x y) x) 5 6))
  5)
(test 'lambda-app-2
  (ev #'((lambda (x y) y) 5 6))
  6)

(test 'let-1
  (ev #'(let ((x 8)) x))
  8)
(test 'let-2
  (ev #'(let ((x 9)) (let ((x 20)) x)))
  20)
(test 'let-3
  (ev #'(let ((x 9)) (let ((y 20)) x)))
  9)
(test 'let-4
  (ev #'(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
  10)
(test 'let-5
  (ev #'(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
  (let ((x 10) (y 4)) (let ((x 11) (y x)) y)))

(test 'internal-defs-1
  (ev #'(let ((x 1) (y 7) (z 33))
          (define u 88)
          (define z y)
          6
          (begin (define a 5) (define w 4))
          z))
  7)
(test 'internal-defs-2
  (ev #'(let ((x 1) (y 7) (z 33))
          (define y 88)
          (define z y)
          6
          (begin (define a 5) (define w 4))
          z))
  88)
(test 'internal-defs-3
  (ev #'(let ((x 1) (y 7) (z 33))
          (define y 88)
          (define z y)
          6
          (begin (define a 5) (define w 4))
          a))
  5)
(test 'internal-defs-4
  (ev #'(let ((x 1) (y 7) (z 33))
          (define y 88)
          (define (z x) x)
          6
          (begin (define a 5) (define w (z y)))
          w))
  88)

(test 'misc-1
  (ev #'((lambda (w #f x #f y . z)
           (if (x y z '(a ... z))
             'true
             'false))
         1 2 (lambda x x) 4 5 6 7))
  'true)
(test 'misc-2
  (ev #'((lambda (w #f x #f y . z)
           (if (x y z '(a ... z))
             'true
             'false))
         1 2 (lambda x #f) 4 5 6 7))
  'false)
(test 'misc-2
  (ev #'((lambda (w #f x #f y . z)
           (if 'true
             (x y z '(a ... z))
             'false))
         1 2 (lambda x x) 4 5 6 7))
  '(5 (6 7) (a ... z)))
