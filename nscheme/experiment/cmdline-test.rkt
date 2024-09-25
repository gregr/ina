#lang racket/base

(define-syntax-rule (show expr)
  (begin
    (write 'expr)
    (newline)
    (write '=>)
    (newline)
    (write expr)
    (newline)
    (newline)))

(show (current-directory))
(show (find-system-path 'orig-dir))
(show (find-system-path 'run-file))
(show (find-system-path 'exec-file))
(show (current-command-line-arguments))
