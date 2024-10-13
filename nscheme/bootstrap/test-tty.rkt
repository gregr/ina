#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt" racket/pretty)
(module nscm:base racket
  (provide (all-defined-out))
  (require "../platform/racket/nscheme.rkt" (for-syntax racket/list)))
(require 'nscm:base (for-syntax 'nscm:base))
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

;; TODO: convert this entire test to nscheme
(define tty
  (let ()
    (define (command path . arg*)
      (let ((result (call-with-output-bytevector
                     (lambda (out)
                       (let ((in (open-file-istream "/dev/tty")))
                         (host-process-wait
                          (host-process in out out
                                        (find-file/env host-environment path) arg* #f))
                         (istream-close in))))))
        (pretty-write `(result: ,result))
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
        (display (string-append csi:cursor-save
                                csi:display-save
                                (csi:cursor-move-to 0 0)
                                csi:display-clear-full))
        (flush-output))
      (lambda () body ...)
      (lambda ()
        (display (string-append csi:display-restore
                                csi:cursor-restore))
        (flush-output)
        (tty 'stty-set settings)))))
(define-syntax-rule (with-tty-cursor-hidden body ...)
  (dynamic-wind
    (lambda () (display csi:cursor-hide))
    (lambda () body ...)
    (lambda () (display csi:cursor-show))))

(define (display/style style s)
  (display (sgr->csi style))
  (display s)
  (display (sgr->csi (append sgr:reset))))

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
    (display "\r\n")
    (displayln/style '()     "testing (1 2 3)")
    (displayln/style style.0 "testing (1 2 3)")
    (displayln/style style.1 "testing (1 2 3)")
    (displayln/style style.2 "testing (1 2 3)")
    (displayln/style style.3 "testing (1 2 3)")
    (displayln/style '()     "testing (1 2 3)")
    (displayln/style '() "\r\nHit any key...")
    (read-byte)))
