;;;;;;;;;;;;;;;;
;;; Printing ;;;
;;;;;;;;;;;;;;;;

(define (make-printer print newline) (vector print newline))
(define (printer-print   p text) ((vector-ref p 0) text))
(define (printer-newline p)      ((vector-ref p 1)))

(define (printer:port port)
  (define (text-write t) (let ((len (bytevector-length t))) (oport-write port t 0 len len)))
  (make-printer (lambda (text) (text-write text)) (lambda () (text-write #"\n"))))

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

(define (make-layout place begin-group end-group separate)
  (vector place begin-group end-group separate))
(define (layout-place       l width text) ((vector-ref l 0) width text))
(define (layout-begin-group l indent)     ((vector-ref l 1) indent))
(define (layout-end-group   l)            ((vector-ref l 2)))
(define (layout-separate    l)            ((vector-ref l 3)))

(define (layout:simple printer)
  (make-layout
    (lambda (width text) (printer-print printer text))
    (lambda (indent)     (values))
    (lambda ()           (values))
    (lambda ()           (printer-print printer #" "))))

;;;;;;;;;;;;;;;;;
;;; Rereading ;;;
;;;;;;;;;;;;;;;;;
;;; A rereader processes a stream of tokens coming from a structured data source.
;;; This is slightly different from a reader, which processes a stream of tokens coming from an
;;; unstructured data source, such as text.
;;; The rereader interface benefits from the structured source by including a datum parameter in
;;; more operations than the reader interface, replacing type parameters, and making shape
;;; parameters unnecessary.
;;; While the reader interface includes a parameter for unstructured source location information
;;; in all of its operations, the rereader interface replaces this with a text parameter.
;;; - reader interface:
;;;   - (atom          location datum)
;;;   - (prefix        location type)
;;;     - type is the symbol that was abbreviated: quote quasiquote unquote etc.
;;;   - (dot           location)
;;;   - (left-bracket  location shape type)
;;;     - type is one of: list vector bytevector
;;;     - shape is one of: round square curly
;;;   - (right-bracket location shape)
(define (make-rereader atom prefix dot left-bracket right-bracket)
  (vector atom prefix dot left-bracket right-bracket))
(define (rereader-atom          r text datum) ((vector-ref r 0) text datum))
(define (rereader-prefix        r text datum) ((vector-ref r 1) text datum))
(define (rereader-dot           r text)       ((vector-ref r 2) text))
(define (rereader-left-bracket  r text datum) ((vector-ref r 3) text datum))
(define (rereader-right-bracket r text)       ((vector-ref r 4) text))

(define (rereader:layout/transform l transform)
  (mlet ((separate? #f))
    (define (separate) (when separate? (layout-separate l)))
    (define (text-place type datum t) (layout-place l (utf8-length t) (transform type datum t)))
    (make-rereader
      (lambda (text datum) (separate) (text-place 'atom   datum text) (set! separate? #t))
      (lambda (text datum) (separate) (text-place 'prefix datum text) (set! separate? #f))
      (lambda (text)       (separate) (text-place 'dot    #f    text) (set! separate? #t))
      (lambda (text datum) (let ((len (utf8-length text)))
                             (separate)
                             (layout-begin-group l len)
                             (layout-place       l len (transform 'left-bracket datum text))
                             (set! separate? #f)))
      (lambda (text)
        (text-place 'right-bracket #f text)
        (layout-end-group l)
        (set! separate? #t)))))
(define (rereader:layout l) (rereader:layout/transform l (lambda (type datum text) text)))

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
