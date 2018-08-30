#lang racket/base
(require
  racket/include
  racket/list
  racket/runtime-path
  "filesystem.rkt"
  "nscheme-module.rkt"
  )

(define tests-total 0)
(define test-failures '())

(define (test-report)
  (define tests-failed (length test-failures))
  (define tests-passed (- tests-total tests-failed))
  (printf "********************************\nTests passed: ~a out of ~a\n"
          tests-passed tests-total)
  (unless (= tests-passed tests-total)
    (printf "Tests failed: ~a out of ~a\n" tests-failed tests-total)
    (printf "~s\n" test-failures)))

(define (test name actual expected)
  (printf "Testing ~a: " name)
  (set! tests-total (+ tests-total 1))
  (cond ((equal? expected actual) (printf "Succeeded.\n"))
        (else (printf "Failed.\nExpected: ~s\nActual: ~s\n****************\n"
                      expected actual)
              (set! test-failures (cons name test-failures)))))

(define db:lib (with-input-from-file db:lib-path read))
(define lib:data    (library-get db:lib 'data))
(define lib:nscheme (library-get db:lib 'nscheme))

(define env:data    (link/module* '() (map cdr lib:data)))
(define env:nscheme (link/module* env:data (map cdr lib:nscheme)))

(let ()
  (map (lambda (t) (t test))
       (reverse (map cdr (filter (lambda (rib) (string=? "test!" (car rib)))
                                 env:nscheme))))
  (test-report))
