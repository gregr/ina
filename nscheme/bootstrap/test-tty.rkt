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
(define tty
  (let ()
    (define (command path . arg*)
      (let ((result (call/oport:bytevector
                     (lambda (out)
                       (let ((in (iport:file "/dev/tty")))
                         (host-process-wait
                          (host-process in out out
                                        (find-file/env host-environment path) arg* #f))
                         (iport-close in))))))
        (rkt:pretty-write `(result: ,result))
        result))
    (define (tput        x) (command "tput" x))
    (define (tput-number x) (utf8->number (bytevector-rtrim (tput x) 10)))
    (define (stty     . x*) (apply command "stty" x*))
    (lambda (method . arg*)
      (apply
       (case method
         ;; NOTE: ec:display-size can report these, but reading the report may be inconvenient
         ((lines)       (lambda ()  (tput-number "lines")))
         ((columns)     (lambda ()  (tput-number "cols")))
         ;; NOTE: these don't seem necessary due to existing escape codes
         ;((clear)       (lambda ()  (command "clear"))) ; \e[2J
         ;((save)        (lambda ()  (tput "smcup")))    ; \e[?47h
         ;((restore)     (lambda ()  (tput "rmcup")))    ; \e[?47l
         ;((cursor-show) (lambda ()  (tput "cnorm")))    ; \e[?25h
         ;((cursor-hide) (lambda ()  (tput "civis")))    ; \e[?25l
         ((stty-ref)    (lambda ()  (stty "-g")))
         ((stty-set)    (lambda (s) (stty s)))
         ((stty-raw)    (lambda ()  (stty "raw"))))
       arg*))))

(define-syntax-rule (with-tty-fresh body ...)
  (let ((settings (tty 'stty-ref)))
    (dynamic-wind
      (lambda ()
        (rkt:display (string-append csi:cursor-save
                                    csi:display-save
                                    (csi:cursor-move-to 0 0)
                                    csi:display-clear-full))
        (flush-output))
      (lambda () body ...)
      (lambda ()
        (rkt:display (string-append csi:display-restore
                                    csi:cursor-restore))
        (flush-output)
        (tty 'stty-set settings)))))
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
    (tty 'stty-raw)
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
