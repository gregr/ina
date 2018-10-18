#lang racket/base
(require
  "filesystem.rkt"
  "module.rkt"
  racket/include
  racket/list
  racket/runtime-path
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

(define (library-modules library-name module-names)
  (map (lambda (module-name)
         (eval/module (nscheme-module
                        (read/file (library-path library-name module-name)))))
       module-names))
(define env:data
  (link/module*
    '() (library-modules 'data '(box tagged symbol assoc compare))))
(define env:nscheme
  (link/module*
    env:data (library-modules 'nscheme '(data common ast stage base-test))))

(define (plift racket-proc) (lambda (a) (apply racket-proc a)))
(for-each (lambda (t) (t (list (plift test))))
          (reverse (map cdr (filter (lambda (rib) (string=? "test!" (car rib)))
                                    env:nscheme))))
(test-report)
