#lang racket/base
(require
  racket/include
  "filesystem.rkt"
  "nscheme-module.rkt"
  )

(define db:lib
  (for/list ((src '((data box tagged assoc compare)
                    (nscheme data ast ast-eval))))
    (define library-name (car src))
    `(,library-name
       . ,(for/list ((module-name (cdr src)))
            (call-with-input-file
              (local-path
                (build-path "lib" (symbol->string library-name)
                            (string-append (symbol->string module-name)
                                           ".scm")))
              (lambda (in)
                (let loop ((rbody '()))
                  (define datum (ns->s (read in)))
                  (if (eof-object? datum)
                    `(,module-name . ,(nscheme-module (reverse rbody)))
                    (loop (cons datum rbody))))))))))

(call-with-output-file db:lib-path
                       (lambda (out) (write (ns->s/write db:lib) out)))

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
