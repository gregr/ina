#lang racket/base
(provide
  unsafe0-parse
  unsafe0-module
  )

(require
  "linking.rkt"
  "parsing.rkt"
  "term.rkt"
  racket/function
  racket/match
  )

(module+ test
  (require rackunit))

(define (parse-extra senv stx)
  (match stx
    (0 (t-value (v-bit (b-0))))
    (1 (t-value (v-bit (b-1))))
    (_ (error (format "invalid syntax: ~a" stx)))))

(define parse-term (parse parse-extra))
(define parse-val (parse-value parse-term))

(define (parse-if0 senv head tail)
  (define (pthunk stx) ((parse-thunk parse-term) senv stx))
  (match tail
    ((list cnd case0 case1)
     (t-apply
       (t-unpair (parse-term senv cnd)
                 (t-value (v-pair (pthunk case0) (pthunk case1))))
       (t-value (v-unit))))
    (_ (error (format "invalid if0: ~a" `(,head . ,tail))))))

(define unsafe0-specials `(
  (lambda . ,(parse-lambda parse-term))
  (pair   . ,(parse-pair parse-val))
  (unpair . ,(parse-unpair parse-term))
  (if0    . ,parse-if0)
  (let    . ,(parse-let parse-term))
  (let*   . ,(parse-let* parse-term))
  ))

(define unsafe0-senv-empty (senv-new unsafe0-specials))
(define unsafe0-parse (curry parse-term unsafe0-senv-empty))

(module+ test
  (require
    "denotation.rkt"
    )
  (define tt-0 (unsafe0-parse
    '((lambda (x0 x1)
        ((lambda (b0) (unpair b0 (pair () (pair b0 (pair x0 x1)))))
         x0))
      1 0)))
  (check-equal? (denote tt-0) '(1 1 . 0))
  (define tt-1 (unsafe0-parse
    '((lambda (x0 x1)
        ((lambda (b0) (if0 b0 b0 (pair x0 x1)))
         x0))
      1 0)))
  (check-equal? (denote tt-1) '(1 . 0))
  (define tt-2 (unsafe0-parse
    '(let ((x0 1) (x1 0))
       ((lambda (b0) (if0 b0 b0 (pair x0 x1)))
        x0))))
  (check-equal? (denote tt-2) '(1 . 0))
  (define tt-3 (unsafe0-parse
    '(let* ((x0 1) (x1 0) (p0 (pair x0 x1)))
       ((lambda (b0) (if0 (unpair b0 p0) p0 b0))
        x0))))
  (check-equal? (denote tt-3) '(1 . 0))
  )

(define unsafe0-module (curry (link-module unsafe0-parse) '()))
