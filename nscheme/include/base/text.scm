;;; Text values are assumed to be UTF-8-encoded.

;;;;;;;;;;;;;;;;
;;; Printing ;;;
;;;;;;;;;;;;;;;;
;;; To interoperate safely with printer targets that perform compositing, calls to printer-print
;;; must emit text horizontally in a predictable manner.  This means that text passed to
;;; printer-print must not contain control codes or markup that change vertical position or that
;;; nonlocally transform text or other ambient state.  Vertical spacing should be requested by
;;; calling printer-newline.
(define (make-printer print print-byte newline) (vector print print-byte newline))
(define (printer-print      p text) ((vector-ref p 0) text))
(define (printer-print-byte p byte) ((vector-ref p 1) byte))
(define (printer-newline    p)      ((vector-ref p 2)))

(define (printer:port port)
  (make-printer (lambda (t) (let ((len (bytevector-length t))) (oport-write port t 0 len len)))
                (lambda (b) (oport-write-byte port b))
                (lambda ()  (oport-write-byte port 10))))

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;
;;; Layout commands express preferences, not guarantees.  The layout policy is responsible for
;;; determining when and how preferences are realized.
;;; - The layout-indent layout-horizontal and layout-vertical commands specify preferences that are
;;;   specific to the current group.  This means the layout-end-group command will re-establish the
;;;   parent group preferences.
;;; - The layout-indent command sets the indentation column to the current placement position within
;;;   the current group.  This means that if the next placement span would begin at column C, then
;;;   the indentation column is set to (+ C offset).  The indentation column is used as the starting
;;;   position whenever the layout policy decides to add vertical spacing.  The initial indentation
;;;   column is 0, and each new group will begin with its indentation column set to the location
;;;   of its first placement.
;;; - The layout-horizontal and layout-vertical commands indicate the preferred direction to move in
;;;   when layout-separate chooses to introduce spacing.  The layout policy is allowed to completely
;;;   ignore this preference.  The initial preference is horizontal, and each new group will also
;;;   begin with a horizontal preference.
(define (make-layout place separate begin-group end-group indent horizontal vertical)
  (vector place separate begin-group end-group indent horizontal vertical))
(define (layout-place       l text text.display) ((vector-ref l 0) text text.display))
(define (layout-separate    l)                   ((vector-ref l 1)))
(define (layout-begin-group l)                   ((vector-ref l 2)))
(define (layout-end-group   l)                   ((vector-ref l 3)))
(define (layout-indent      l offset)            ((vector-ref l 4) offset))
(define (layout-horizontal  l)                   ((vector-ref l 5)))
(define (layout-vertical    l)                   ((vector-ref l 6)))

(define (layout:single-line printer)
  (make-layout
    (lambda (text text.display) (printer-print      printer text.display))
    (lambda ()                  (printer-print-byte printer 32))
    (lambda ()                  (values))
    (lambda ()                  (values))
    (lambda (offset)            (values))
    (lambda ()                  (values))
    (lambda ()                  (values))))

;;;;;;;;;;;;;;
;;; Writer ;;;
;;;;;;;;;;;;;;
;;; A writer consumes a stream of tokens coming from a structured data source.  Each writer
;;; operation corresponds to a type of token, taking a pair of text parameters, followed by a source
;;; datum parameter when applicable.  The first text parameter contains the original, unadorned
;;; notation.  The second text parameter is the same notation, but it may have been decorated with
;;; additional styling or markup to improve the way it is displayed.
(define (make-writer atom prefix dot left-bracket right-bracket)
  (vector atom prefix dot left-bracket right-bracket))
(define (writer-atom          w text text.display datum) ((vector-ref w 0) text text.display datum))
(define (writer-prefix        w text text.display datum) ((vector-ref w 1) text text.display datum))
(define (writer-dot           w text text.display)       ((vector-ref w 2) text text.display))
(define (writer-left-bracket  w text text.display datum) ((vector-ref w 3) text text.display datum))
(define (writer-right-bracket w text text.display)       ((vector-ref w 4) text text.display))

(define (writer-decorate/sgr w sgr.reset sgr.prefix sgr.dot sgr.bracket atom->sgr)
  (define (decorate text sgr) (bytevector-append sgr text sgr.reset))
  (make-writer
    (lambda (t text.d x) (writer-atom          w t (decorate text.d (atom->sgr x)) x))
    (lambda (t text.d x) (writer-prefix        w t (decorate text.d sgr.prefix)    x))
    (lambda (t text.d)   (writer-dot           w t (decorate text.d sgr.dot)))
    (lambda (t text.d x) (writer-left-bracket  w t (decorate text.d sgr.bracket)   x))
    (lambda (t text.d)   (writer-right-bracket w t (decorate text.d sgr.bracket)))))

(define (writer:layout l)
  (mlet ((separate? #f))
    (define (separate) (when separate? (layout-separate l)))
    (define (place t t.d) (layout-place l t t.d))
    (make-writer
      (lambda (text text.display datum) (separate) (place text text.display) (set! separate? #t))
      (lambda (text text.display datum) (separate) (place text text.display) (set! separate? #f))
      (lambda (text text.display)       (separate) (place text text.display) (set! separate? #t))
      (lambda (text text.display datum)
        (separate)
        (layout-begin-group l)
        (place text text.display)
        (layout-indent l 0)
        (set! separate? #f))
      (lambda (text text.display)
        (place text text.display)
        (layout-end-group l)
        (set! separate? #t)))))

(define (writer:layout/sgr l sgr.reset sgr.prefix sgr.dot sgr.bracket datum->sgr)
  (writer-decorate/sgr (writer:layout l) sgr.reset sgr.prefix sgr.dot sgr.bracket datum->sgr))

;;;;;;;;;;;;;;
;;; Reader ;;;
;;;;;;;;;;;;;;
;;; A reader consumes a stream of tokens coming from an unstructured data source, such as text.
;;; Each reader operation corresponds to a type of token, taking a source location parameter
;;; followed by parameters for any token-specific details, and returning a boolean indicating
;;; whether its driver should continue sending tokens.
(define (make-reader atom prefix dot left-bracket right-bracket datum-comment eof error)
  (vector atom prefix dot left-bracket right-bracket datum-comment eof error))
(define (reader-atom          r loc datum)      ((vector-ref r 0) loc datum))
(define (reader-prefix        r loc type)       ((vector-ref r 1) loc type))
(define (reader-dot           r loc)            ((vector-ref r 2) loc))
(define (reader-left-bracket  r loc shape type) ((vector-ref r 3) loc shape type))
(define (reader-right-bracket r loc shape)      ((vector-ref r 4) loc shape))
(define (reader-datum-comment r loc)            ((vector-ref r 5) loc))
(define (reader-eof           r loc)            ((vector-ref r 6) loc))
(define (reader-error         r loc exception)  ((vector-ref r 7) loc exception))

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

(define (make-notate notation)
  (let* ((notation                 (notation-override notation.default notation))
         (abbreviate-reader-macro? (notation-ref notation '(abbreviate-reader-macro?)))
         (abbreviate-pair?         (notation-ref notation '(abbreviate-pair?)))
         (bracket-index            (case (notation-ref notation '(bracket))
                                     ((40  #"(" "(" round)  0)
                                     ((91  #"[" "[" square) 1)
                                     ((123 #"{" "{" curly)  2)
                                     (=> (lambda (b) "not a bracket" b))))
         (length-prefix?           (notation-ref notation '(length-prefix?)))
         (bytevector-numeric?      (notation-ref notation '(bytevector-numeric?)))
         (implicit-radix           (notation-ref notation '(number implicit-radix)))
         (radix                    (or (notation-ref notation '(number radix)) implicit-radix))
         (capitalize-digits?       (notation-ref notation '(number capitalize-digits?)))
         (fraction                 (notation-ref notation '(number fraction)))
         (exponent-above           (notation-ref notation '(number exponent above)))
         (exponent-below           (notation-ref notation '(number exponent below)))
         (text.left-bracket        (vector-ref '#(#"(" #"[" #"{") bracket-index))
         (text.right-bracket       (vector-ref '#(#")" #"]" #"}") bracket-index))
         (text.null                (bytevector-append text.left-bracket text.right-bracket)))
    (lambda (writer x)
      (let notate ((x x))
        (define (atom         text datum) (writer-atom          writer text text datum))
        (define (prefix       text datum) (writer-prefix        writer text text datum))
        (define (dot)                     (writer-dot           writer #"." #"."))
        (define (left-bracket text datum) (writer-left-bracket  writer text text datum))
        (define (right-bracket)           (writer-right-bracket writer text.right-bracket
                                                                text.right-bracket))
        (cond
          ((null? x)  (atom text.null x))
          ((not x)    (atom #"#f"     x))
          ((eq? x #t) (atom #"#t"     x))
          ((pair? x)  (let ((abbrev (and abbreviate-reader-macro?
                                         (symbol? (car x))
                                         (pair? (cdr x))
                                         (null? (cddr x))
                                         (case (car x)
                                           ((quote)             #"'")
                                           ((quasiquote)        #"`")
                                           ((unquote)           #",")
                                           ((unquote-splicing)  #",@")
                                           ((syntax)            #"#'")
                                           ((quasisyntax)       #"#`")
                                           ((unsyntax)          #"#,")
                                           ((unsyntax-splicing) #"#,@")
                                           (else                #f)))))
                        (if abbrev
                            (begin (prefix abbrev x) (notate (cadr x)))
                            (begin (left-bracket text.left-bracket x)
                                   (notate (car x))
                                   (if abbreviate-pair?
                                       (let loop ((x (cdr x)))
                                         (unless (null? x)
                                           (cond ((pair? x) (notate (car x)) (loop (cdr x)))
                                                 (else      (dot) (notate x)))))
                                       (begin (dot) (notate (cdr x))))
                                   (right-bracket)))))
          ((vector? x)
           (let ((len (vector-length x)))
             (left-bracket (bytevector-append #"#" (if length-prefix? (number->utf8 len) #"")
                                              text.left-bracket)
                           x)
             (unless (= len 0)
               (notate (vector-ref x 0))
               (range-for-each (lambda (i) (notate (vector-ref x i)))
                               1
                               (if (and (< 1 len) length-prefix?)
                                   (let ((last (vector-ref x (- len 1))))
                                     (let loop ((i (- len 2)))
                                       (if (equal? (vector-ref x i) last)
                                           (if (< 0 i) (loop (- i 1)) 1)
                                           (+ i 2))))
                                   len))))
           (right-bracket))
          ((bytevector? x)
           (let* ((len         (bytevector-length x))
                  (text.prefix (bytevector-append #"#" (if length-prefix? (number->utf8 len) #"")))
                  (len.prefix  (if (and (< 1 len) length-prefix?)
                                   (let ((last (bytevector-ref x (- len 1))))
                                     (let loop ((i (- len 2)))
                                       (if (equal? (bytevector-ref x i) last)
                                           (if (< 0 i) (loop (- i 1)) 1)
                                           (+ i 2))))
                                   len)))
             (if bytevector-numeric?
                 (begin (left-bracket (bytevector-append text.prefix #"vu8" text.left-bracket) x)
                        (unless (= len 0)
                          (notate (bytevector-ref x 0))
                          (range-for-each (lambda (i) (notate (bytevector-ref x i)))
                                          1 len.prefix))
                        (right-bracket))
                 (atom
                   #"#\"TODO\""
                   x))))

          ((string? x)
           (atom
             #"\"TODO\""
             x))
          ((symbol? x)
           (atom (let* ((bv  (string->utf8 (symbol->string x)))
                        (len (bytevector-length bv)))
                   (cond
                     ((= len 0)      #"||")
                     ((eqv? bv #".") #"|.|")
                     (else #"TODO")))
                 x))
          ((number? x)      (atom
                              ;; TODO: use notation options
                              (number->utf8 x)
                              x))
          ((mbytevector? x) (atom #"#<mbytevector>" x))
          ((mvector? x)     (atom #"#<mvector>"     x))
          ((procedure? x)   (atom #"#<procedure>"   x))
          (else             (atom #"#<unknown>"     x)))))))
(define notate (make-notate notation.empty))

;; TODO: move this example
#;(begin
  (notate (writer:layout (layout:single-line (printer:port standard-output-port)))
          '(() (0) 1 '2 three "four" #"fiveeee" #(6 7 7 7) #t #f . 10))
  (oport-write-byte standard-output-port 10)
  ((make-notate '((abbreviate-reader-macro? . #t)
                  (abbreviate-pair? . #f)
                  (bracket . #"[")
                  (length-prefix? . #t)
                  (bytevector-numeric? . #t)))
   (writer:layout (layout:single-line (printer:port standard-output-port)))
   '(() (0) 1 '2 three "four" #"fiveeee" #(6 7 7 7) #t #f . 10))
  (oport-write-byte standard-output-port 10)
  (notate (writer:layout/sgr (layout:single-line (printer:port standard-output-port))
                             #"\e[0m"
                             #"\e[33;5m"
                             #"\e[31;5m"
                             #"\e[32m"
                             (lambda (datum)
                               (cond ((symbol? datum)  #"\e[34m")
                                     ((number? datum)  #"\e[35m")
                                     ((boolean? datum) #"\e[33m")
                                     ((null? datum)    #"\e[32m")
                                     (else             #"\e[36m"))))
          '(() (0) 1 '2 three "four" #"fiveeee" #(6 7 7 7) #t #f . 10))
  (oport-write-byte standard-output-port 10)
  ((make-notate '((abbreviate-reader-macro? . #t)
                  (abbreviate-pair? . #f)
                  (bracket . #"[")
                  (length-prefix? . #t)
                  (bytevector-numeric? . #t)))
   (writer:layout/sgr (layout:single-line (printer:port standard-output-port))
                      #"\e[0m"
                      #"\e[33;5m"
                      #"\e[31;5m"
                      #"\e[32m"
                      (lambda (datum)
                        (cond ((symbol? datum)  #"\e[34m")
                              ((number? datum)  #"\e[35m")
                              ((boolean? datum) #"\e[33m")
                              ((null? datum)    #"\e[32m")
                              (else             #"\e[36m"))))
   '(() (0) 1 '2 three "four" #"fiveeee" #(6 7 7 7) #t #f . 10))
  (oport-write-byte standard-output-port 10)
  )
