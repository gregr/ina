#lang racket/base
(require
  racket/include
  "nscheme.rkt"
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

(define-syntax include-list
  (syntax-rules () ((_ fname ...) (list (include fname) ...))))

(define env (link/module '() (include-list "lib/assoc.scm"
                                           "lib/compare.scm")))

(let ()
  (map (lambda (t) (t test))
       (map cdr (filter (lambda (rib) (string=? 'test! (car rib))) env)))
  (test-report))
