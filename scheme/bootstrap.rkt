#lang racket/base
(require
  racket/include
  "nscheme.rkt"
  )

;; TODO: do something like this:

;(define nscheme-compile (include "nscheme.scm"))

;(with-output-to-file
  ;"nscheme.scm.rkt"
  ;(lambda ()
    ;(write (nscheme-compile 'racket
             ;(with-input-from-string "nscheme.scm" read)))))

;; and then...
;; * sanity check bootstrapped nscheme.scm.rkt
;; * reboot into bootstrapped nscheme.scm.rkt
;; * compile to javascript, python, C, etc.
;; * build initial DB
;; * copy the .scm code as data into the initial DB
