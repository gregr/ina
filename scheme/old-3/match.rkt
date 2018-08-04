#lang racket/base
(provide
  match
  )

(require
  "match-new.rkt"
  )

(define-syntax match
  (syntax-rules ()
    ((_ body ...) (match-new body ...))))
