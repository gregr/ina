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
  (display (sgr*->ec style))
  (display s)
  (display (sgr*->ec (list sgr:reset))))

(define (displayln/style style s)
  (display/style style s)
  (display "\r\n"))

(with-tty-fresh
  (with-tty-cursor-hidden
    (tty 'stty-raw)

    (define style.0 (list sgr:color-fg:magenta sgr:color-bg:yellow
                          sgr:bold+
                          sgr:blink+
                          ))
    (define style.1 (list sgr:color-fg:green
                          sgr:color-bg:red
                          sgr:underline+
                          sgr:invert+
                          ))

    (displayln/style '()     "testing (1 2 3)")
    (displayln/style style.0 "testing (1 2 3)")
    (displayln/style style.1 "testing (1 2 3)")
    (displayln/style '()     "testing (1 2 3)")

    (displayln/style '() "\r\nHit any key...")
    (read-byte)))
