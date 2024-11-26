;;;;;;;;;;;;;;;;
;;; Notation ;;;
;;;;;;;;;;;;;;;;
;; structure:
;; - abbreviate-reader-macro?
;;   - for: quote quasiquote unquote unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing
;; - abbreviate-pair?
;; - bracket: `( [ {`
;; - length-prefix?
;; - bytevector-numeric?
;; - number
;;   - implicit-radix
;;     - determines which radix does not need a prefix
;;     - read should be given an impicit-radix to decide how to recognize unprefixed numbers
;;   - radix: #f 2 8 10 16
;;     - #f (the default) to use implicit-radix
;;   - capitalize-digits?
;;   - fraction
;;     - ratio
;;     - decimal (falls back to ratio if there would be unallowed repetition)
;;     - decimal/repeat
;;   - exponent
;;     - above: #f or nonnegative integer
;;       - use scientific notation for decimal fractions when (>= (abs value) (expt radix above))
;;       - #f is infinity
;;     - below: #f or nonpositive integer
;;       - use scientific notation for decimal fractions when (< 0 (abs value) (expt radix below))
;;       - #f is infinity
(define notation.empty '())
(define notation.default
  '((abbreviate-reader-macro? . #f)
    (abbreviate-pair? . #t)
    (bracket . 40)  ; "("
    (length-prefix? . #f)
    (bytevector-numeric? . #f)
    (number (implicit-radix . 10) (radix . #f) (capitalize-digits? . #f) (fraction . ratio)
            (exponent (above . #f) (below . -3)))))

(define (notation-ref notation key)
  (atree-ref/k notation key (lambda () (error "missing notation key" key notation)) (lambda (v) v)))
(define (notation-override notation notation.override) (atree-replace notation notation.override))
