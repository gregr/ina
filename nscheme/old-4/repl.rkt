#lang racket/base
(provide repl)

(require
  "scheme.rkt"
  readline
  readline/pread
  )

(define (repl)
  (define form (parameterize ((readline-prompt #"REPL> ")) (read)))
  (if (eof-object? form) (printf "exiting REPL\n")
    (begin (printf "~s\n" (scheme-eval form))
           (repl))))
