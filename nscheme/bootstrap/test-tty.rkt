#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt"
  (prefix-in rkt: racket/base) (prefix-in rkt: racket/pretty))
(module nscm:base racket
  (provide (all-defined-out))
  (require "../platform/racket/nscheme.rkt" (for-syntax racket/list)))
(require 'nscm:base (for-syntax 'nscm:base))
(print-as-expression #f)
(rkt:pretty-print-abbreviate-read-macros #f)

;; TODO: convert this entire test to nscheme
(define-syntax-rule (with-tty-fresh body ...)
  (let ((settings (stty-ref)))
    (dynamic-wind
     (lambda ()
       (rkt:display (bytevector-append csi:cursor-save
                                       csi:display-save
                                       (csi:cursor-move 1 1)
                                       csi:display-clear-screen))
       (flush-output))
     (lambda () body ...)
     (lambda ()
       (rkt:display (bytevector-append csi:display-restore csi:cursor-restore))
       (flush-output)
       (stty-set settings)))))
(define-syntax-rule (with-tty-cursor-hidden body ...)
  (dynamic-wind
    (lambda () (rkt:display csi:cursor-hide))
    (lambda () body ...)
    (lambda () (rkt:display csi:cursor-show))))

(define (display/sgr sgr s)
  (rkt:display sgr)
  (rkt:display s)
  (rkt:display sgr:reset))

(define (displayln/sgr sgr s)
  (display/sgr sgr s)
  (rkt:display "\r\n"))

(with-tty-fresh
  (with-tty-cursor-hidden
    (stty-raw)
    (define sgr.0 (make-sgr))
    (define sgr.1 (make-sgr
                   sgra:color-simple-fg:magenta
                   sgra:color-simple-bg:yellow
                   sgra:bold+
                   sgra:blink+
                   ))
    (define sgr.2 (make-sgr
                   sgra:color-simple-fg:green
                   sgra:color-simple-bg:red
                   sgra:underline+
                   sgra:invert+
                   ))
    (define sgr.3 (make-sgr
                   (sgra:color-6cube-fg 3 4 5)
                   (sgra:color-24gray-bg 7)
                   ))
    (define sgr.4 (make-sgr
                   (sgra:color-6cube-fg 0 0 3)
                   (sgra:color-6cube-bg 2 2 0)
                   sgra:invert+
                   sgra:blink+
                   sgra:underline+
                   ))
    (rkt:display "\r\n")
    (displayln/sgr sgr.0 "testing (1 2 3)")
    (displayln/sgr sgr.1 "testing (1 2 3)")
    (displayln/sgr sgr.2 "testing (1 2 3)")
    (displayln/sgr sgr.3 "testing (1 2 3)")
    (displayln/sgr sgr.4 "testing (1 2 3)")
    (displayln/sgr sgr.0 "testing (1 2 3)")
    (displayln/sgr sgr.0 "\r\nHit any key...")
    (read-byte)))
