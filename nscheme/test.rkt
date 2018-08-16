#lang racket/base
(require
  racket/include
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

(define lib (cdr (assoc 'lib (with-input-from-file
                               (local-path "sources.db.scm") read))))

(define env (link/module '() (map (lambda (a) (eval/module (cdr a))) lib)))

(let ()
  (map (lambda (t) (t test))
       (map cdr (filter (lambda (rib) (string=? "test!" (car rib))) env)))
  (test-report))
