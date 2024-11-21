;;;;;;;;;;;;;
;;; Style ;;;
;;;;;;;;;;;;;
;; structure:
;; - layout
;;   - width: #f (for infinity) or nonnegative integer
;;     - default to #f, which means indent is irrelevant
;;   - indent
;;     - default to 1
;;     - how much to indent beyond the column of a surrounding opening bracket
;; - notation
;;   - abbreviate-reader-macros?
;;     - for: quote quasiquote unquote unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing
;;   - abbreviate-pairs?
;;   - bracket: `( [ {`
;;   - length-prefix?
;;   - bytevector-numeric?
;;   - number
;;     - implicit-radix
;;       - determines which radix does not need a prefix
;;       - read should be given an impicit-radix to decide how to recognize unprefixed numbers
;;     - radix: #f 2 8 10 16
;;       - #f (the default) to use implicit-radix
;;     - capitalize-digits?
;;     - fraction
;;       - ratio
;;       - decimal (falls back to ratio if there would be unallowed repetition)
;;     - allow-decimal-repeat?
;;     - exponent
;;       - above: #f or nonnegative integer
;;         - use scientific notation for decimal fractions when (>= (abs value) (expt radix above))
;;         - #f is infinity
;;       - below: #f or nonpositive integer
;;         - use scientific notation for decimal fractions when (< 0 (abs value) (expt radix below))
;;         - #f is infinity
(define style.empty '())
(define style.default
  '((layout (width . #f) (indent . 1))
    (notation
      (abbreviate-reader-macros? . #f)
      (abbreviate-pairs? . #t)
      (bracket . 40)  ; "("
      (length-prefix? . #f)
      (bytevector-numeric? . #f)
      (number (implicit-radix . 10) (radix . #f) (capitalize-digits? . #f)
              (fraction . ratio) (allow-decimal-repeat? . #f)
              (exponent (above . #f) (below . -3))))))
(define style.pretty '((layout (width . 80)) (notation (abbreviate-reader-macros? . #t))))

(define (style-ref style key*)
  (atree-ref/k style key* (lambda () (error "missing key in style" key* style)) (lambda (v) v)))
(define (style-override style style.override) (atree-replace style style.override))
