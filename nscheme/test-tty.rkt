#lang racket/base
(require "nscheme.rkt" racket/include racket/list racket/pretty racket/string)
(module nscm:base racket
  (provide (all-defined-out))
  (require "nscheme.rkt" (for-syntax racket/list))
  (include "base.scm"))
(require 'nscm:base (for-syntax 'nscm:base))
(include "tty.scm")
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-syntax-rule (with-tty-fresh body ...)
  (let ((settings (tty 'stty-ref)))
    (dynamic-wind
      (lambda ()
        (display (string-append ec:cursor-save
                                ec:display-save
                                (ec:cursor-move-to 0 0)
                                ec:display-clear-full))
        (flush-output))
      (lambda () body ...)
      (lambda ()
        (display (string-append ec:display-restore
                                ec:cursor-restore))
        (flush-output)
        (tty 'stty-set settings)))))
(define-syntax-rule (with-tty-cursor-hidden body ...)
  (dynamic-wind
    (lambda () (display ec:cursor-hide))
    (lambda () body ...)
    (lambda () (display ec:cursor-show))))

(define (display/style style s)
  (display (sgr->ec style))
  (display s)
  (display (sgr->ec (append sgr:reset))))

(define (displayln/style style s)
  (display/style style s)
  (display "\r\n"))

(with-tty-fresh
  (with-tty-cursor-hidden
    (tty 'stty-raw)

    (define style.0 (append
                      sgr:color-fg:magenta
                      sgr:color-bg:yellow
                      sgr:bold+
                      sgr:blink+
                      ))
    (define style.1 (append
                      sgr:color-fg:green
                      sgr:color-bg:red
                      sgr:underline+
                      sgr:invert+
                      ))
    (define style.2 (append
                      (sgr:color-rgb6-fg 3 4 5)
                      (sgr:color-gray-bg 7)
                      ))
    (define style.3 (append
                      (sgr:color-rgb6-fg 0 0 3)
                      (sgr:color-rgb6-bg 2 2 0)
                      sgr:invert+
                      sgr:blink+
                      sgr:underline+
                      ))

    (displayln/style '()     "testing (1 2 3)")
    (displayln/style style.0 "testing (1 2 3)")
    (displayln/style style.1 "testing (1 2 3)")
    (displayln/style style.2 "testing (1 2 3)")
    (displayln/style style.3 "testing (1 2 3)")
    (displayln/style '()     "testing (1 2 3)")

    (displayln/style '() "\r\nHit any key...")
    (read-byte)))
