#lang racket/base
(provide
  )

(require
  "unsafe0.rkt"
  )

(define std0
  (unsafe0-parse
    '(let* ((identity (lambda (x) x))
            (const    (lambda (k _) k))
            (compose  (lambda (f g x) (f (g x))))
            (fix (lambda (f) ((lambda (d) (d d))
                              (lambda (x) (f (lambda (a) (x x a)))))))
            )
       (pair identity (pair const (pair compose (pair fix ())))))))
