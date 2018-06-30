#lang racket/base
(provide repl)

(require
  "eval-ast.rkt"
  "scheme.rkt"
  readline
  readline/pread
  )

(define (repl)
  (define form (parameterize ((readline-prompt #"REPL> ")) (read)))
  (if (eof-object? form) (printf "exiting REPL\n")
    (begin (printf "~s\n" (eval-ast (expand (program/stdlib form))))
           (repl))))
