;;;;;;;;;;;;;
;;; Style ;;;
;;;;;;;;;;;;;
;; structure:
;; - layout
;;   - margin
;;     - #f (default) for current column position
;;   - width: #f (for infinity) or nonnegative integer
;;     - default to #f, which means indent and compactness are irrelevant
;;   - indent (can be 0 for extra compactness)
;;     - default to 1
;;     - how much to indent beyond the column of a surrounding opening bracket
;;     - 0 means that elements can even appear to the left of a surrounding opening bracket
;;   - compact? (only applicable if width is a number)
;;     - default to #t
;;     - if #f, then a typical "pretty" layout is enabled, which means that vertical mode persists
;;       for elements of vertical compound data
;; - abbreviate
;;   - quote quasiquote unquote unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing
;; - notation
;;   - boolean?
;;     - capitalize?
;;     - word? (as in #true and #false or #TRUE and #FALSE)
;;   - null?
;;     - bracket: `( [ {`
;;   - pair?
;;     - bracket: `( [ {`
;;     - list? (#t by default to minimize dots)
;;   - vector?
;;     - bracket: `( [ {`
;;     - length-prefix?
;;     - compress-suffix?
;;       - only applicable when length-prefix? is true
;;       - if a length-prefix is longer than the number of written elements, the unwritten elements
;;         are the same as the last written element
;;   - bytevector?
;;     - or bracket: #f or `( [ {`
;;       - as in #vu8 etc.
;;       - #f for #"etc."
;;     - length-prefix?
;;     - compress-suffix?
;;   - string?
;;     - number?: #f or a number? style subset
;;       - only integer-relevant options will be used
;;   - number?
;;     - implicit-radix
;;       - determines which radix does not need a prefix
;;       - read should be given an impicit-radix to decide how to recognize unprefixed numbers
;;     - radix: #f 2 8 10 16
;;       - #f (the default) to use implicit-radix
;;     - capitalize-radix-prefix?
;;     - capitalize-digits?
;;     - fraction
;;       - ratio
;;       - decimal (falls back to ratio if there would be unallowed repetition)
;;     - allow-decimal-repeat?
;;     - exponent
;;       - prefix: e E x X
;;         - if e or E, switch to x or X respectively when radix is 16
;;       - fraction?
;;       - above: #f or nonnegative integer
;;         - use scientific notation when (>= (abs value) (expt radix above))
;;         - #f is infinity
;;       - below: #f or nonpositive integer
;;         - use scientific notation when (< 0 (abs value) (expt radix below))
;;         - #f is infinity
(define style.default
  '((layout
      (margin   . #f)
      (width    . #f)
      (indent   . 1)
      (compact? . #t))
    (abbreviate
      (quote             . #t)
      (quasiquote        . #t)
      (unquote           . #t)
      (unquote-splicing  . #t)
      (syntax            . #t)
      (quasisyntax       . #t)
      (unsyntax          . #t)
      (unsyntax-splicing . #t))
    (notation
      (boolean?    (capitalize? . #f) (word? . #f))
      (null?       (bracket . 40))  ; "("
      (pair?       (bracket . 40) (list? . #t))
      (vector?     (bracket . 40) (length-prefix? . #f) (compress-suffix? . #f))
      (bytevector? (bracket . #f) (length-prefix? . #f) (compress-suffix? . #f))
      (string?     (number? . #f))
      (number?     (implicit-radix . 10) (radix . #f) (capitalize-radix-prefix? . #f)
                   (fraction . ratio) (allow-decimal-repeat? . #f)
                   (exponent (prefix    . 101)  ; "e"
                             (fraction? . #f)
                             (above     . #f)
                             (below     . -3))))))

(define (style-override style style.override)
  (let loop ((current* style) (override* style.override))
    (append
      (map (lambda (override)
             (let* ((key (car override)) (value (cdr override)) (current (assq key current*)))
               ;; assumes leaf option values are never pairs or null
               (if (and current (pair? (cdr current)))
                   (cond ((pair? value) (cons key (loop (cdr current) value)))
                         ((null? value) current)
                         (else          override))
                   override)))
           override*)
      (filter (lambda (kv) (not (assq (car kv) override*))) current*))))
