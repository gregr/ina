;;; Text values are assumed to be UTF-8-encoded.

;;;;;;;;;;;;;;;;
;;; Printing ;;;
;;;;;;;;;;;;;;;;
;;; To interoperate safely with printer targets that perform compositing, calls to printer-print
;;; must emit text horizontally in a predictable manner.  This means that text passed to
;;; printer-print must not contain control codes or markup that change vertical position or that
;;; nonlocally transform text or other ambient state.  Vertical spacing should be requested by
;;; calling printer-newline.
(define (make-printer print newline) (vector print newline))
(define (printer-print   p text) ((vector-ref p 0) text))
(define (printer-newline p)      ((vector-ref p 1)))

(define (printer:port port)
  (make-printer (lambda (t) (let ((len (bytevector-length t))) (oport-write port t 0 len len)))
                (lambda ()  (oport-write-byte port 10))))

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;
;;; Layout commands express preferences, not guarantees.  The layout policy is responsible for
;;; determining when and how preferences are realized.
;;; - The layout-place command requests that its first text value parameter be presented, with a
;;;   preference that it have the appearance of the second text value parameter.  The first text
;;;   parameter should contain unadorned text, while the second text parameter should be equivalent
;;;   in textual meaning, but optionally decorated with additional styling or markup to improve the
;;;   way it is displayed.
;;; - The layout-space, layout-newline and layout-space^newline commands request separation, and
;;;   respectively indicate a preference for spacing in the horizontal or vertical direction, or no
;;;   preference at all for layout-space^newline.  Whether the preferred direction is used or not is
;;;   decided by the layout policy.
;;; - The layout-group-begin and layout-group-end commands form groups of aligned lines of text.  A
;;;   new group is specified with an indentation amount that will be calculated relative to its
;;;   first line, and that will be applied to subsequent lines.
;;; - Assumptions:
;;;   - A layout-group-end can only be followed by layout-newline, layout-space^newline, or another
;;;     layout-group-end.
;;;   - Groups will not be pathologically nested.
;;;   - Incomplete groups may prevent layout effects from being observed.
;;;   - Top-level uses of layout-space^newline followed by layout-place or layout-space will
;;;     eventually be terminated with layout-newline, layout-space^newline, or a complete group.
;;;   - In the case of a terminating layout-space^newline, its effect might never be observed.
;;;     - But layout effects preceding the layout-space^newline will be observed.
(define (make-layout place group-begin group-end space newline space^newline)
  (vector place group-begin group-end space newline space^newline))
(define (layout-place         l text text.display) ((vector-ref l 0) text text.display))
(define (layout-group-begin   l indent)            ((vector-ref l 1) indent))
(define (layout-group-end     l)                   ((vector-ref l 2)))
(define (layout-space         l)                   ((vector-ref l 3)))
(define (layout-newline       l)                   ((vector-ref l 4)))
(define (layout-space^newline l)                   ((vector-ref l 5)))

(define (layout:single-line printer)
  (define (space) (printer-print printer #" "))
  (make-layout (lambda (text text.display) (printer-print printer text.display))
               (lambda (indent)            (values))
               (lambda ()                  (values))
               space space space))

(splicing-local
  ((define (make-gbnode prev indent) (let ((gbn (mvector prev #f indent)))
                                       (when prev (gbnode-set-next! prev gbn))
                                       gbn))
   (define (gbnode?          x)        (mvector? x))
   (define (gbnode-prev      gbn)      (mvector-ref  gbn 0))
   (define (gbnode-next      gbn)      (mvector-ref  gbn 1))
   (define (gbnode-set-prev! gbn prev) (mvector-set! gbn 0 prev))
   (define (gbnode-set-next! gbn next) (mvector-set! gbn 1 next))
   (define (gbnode-indent    gbn)      (mvector-ref  gbn 2))
   (define (gbnode-active?   gbn)      (gbnode-indent gbn))
   (define (gbnode-deactivate! gbn)
     (mvector-set! gbn 2 #f)
     (let ((prev (gbnode-prev gbn)) (next (gbnode-next gbn)))
       (when prev (gbnode-set-next! prev next))
       (when next (gbnode-set-prev! next prev))))
   (define (make-placement size content) (cons size content))
   (define (placement-size    p) (car p))
   (define (placement-content p) (cdr p))
   (define (make-seg t*) t*)
   (define (seg-t*        s) s)
   (define (seg-complete? s) (not (ormap (lambda (t) (and (gbnode? t) (gbnode-active? t)))
                                         (seg-t* s))))
   (define fifo.empty '(() . ()))
   (define (fifo-empty? q)   (null? (car q)))
   (define (fifo-top    q)   (car (car q)))
   (define (fifo-pop    q)   (let ((out (cdr (car q))))
                               (if (null? out)
                                   (cons (reverse (cdr q)) '())
                                   (cons out (cdr q)))))
   (define (fifo-push   q x) (if (null? (car q))
                                 (cons (list x) '())
                                 (cons (car q) (cons x (cdr q))))))
  (define (layout:compact printer width)
    (mlet ((single-line-group-depth 1)
           (start* '(0)) (pos.actual 0) (pos.potential 0) (seg* fifo.empty) (rt* #f) (gbn.last #f))
      (define (group-push indent)
        (set! start* (cons (+ pos.actual indent) start*))
        (set! single-line-group-depth (+ single-line-group-depth 1)))
      (define (group-pop)
        (flush)
        (set! start* (cdr start*))
        (when (null? start*) (error "layout-group-end outside a group"))
        (set! single-line-group-depth (max (- single-line-group-depth 1) 0)))
      (define (place size text)
        (set! pos.actual (+ pos.actual size))
        (printer-print printer text))
      (define (newline)
        (let* ((start (car start*))
               (text  (make-mbytevector (+ start 1) 32)))
          (mbytevector-set! text 0 10)
          (printer-print printer (mbytevector->bytevector text))
          (set! pos.potential (- pos.potential (- pos.actual start)))
          (set! pos.actual start)
          (set! single-line-group-depth 1)))
      (define (pop)
        (let* ((seg  (let ((s* seg*))
                       (if (fifo-empty? s*)
                           (let ((seg (make-seg (reverse rt*))))
                             (set! rt* #f)
                             seg)
                           (let ((seg (fifo-top s*)))
                             (set! seg* (fifo-pop s*))
                             seg))))
               (t*   (seg-t* seg))
               (size (+ 1 (foldl (lambda (t size) (if (gbnode? t) size (+ (placement-size t) size)))
                                 0 t*))))
          (if (and (<= (+ pos.actual size) width) (seg-complete? seg))
              (place 1 #" ")
              (begin (set! pos.potential (- pos.potential 1))
                     (newline)))
          (for-each (lambda (t)
                      (if (gbnode? t)
                          (when (gbnode-active? t)
                            (group-push (gbnode-indent t))
                            (when (eq? t gbn.last) (set! gbn.last (gbnode-prev t)))
                            (gbnode-deactivate! t))
                          (place (placement-size t) (placement-content t))))
                    t*)))
      (define (flush)           (let loop () (when rt*                               (pop) (loop))))
      (define (constrain-width) (let loop () (when (and rt* (< width pos.potential)) (pop) (loop))))
      (define (push-placement size t)
        (set! pos.potential (+ pos.potential size))
        (if rt*
            (begin (set! rt* (cons (make-placement size t) rt*))
                   (constrain-width))
            (place size t)))
      (make-layout
        (lambda (text text.display) (push-placement (utf8-length text) text.display))
        (lambda (indent)
          (if rt*
              (let ((gbn (make-gbnode gbn.last indent)))
                (set! gbn.last gbn)
                (set! rt* (cons gbn rt*)))
              (group-push indent)))
        (lambda ()
          (let ((last gbn.last))
            (if last
                (let ((prev (gbnode-prev last)))
                  (set! gbn.last prev)
                  (gbnode-deactivate! last)
                  (unless prev (flush)))
                (group-pop))))
        (lambda () (push-placement 1 #" "))
        (lambda () (flush) (newline))
        (lambda () (if (< 0 single-line-group-depth)
                       (begin (set! pos.potential (+ pos.potential 1))
                              (when rt*
                                (constrain-width)
                                (if gbn.last
                                    (set! seg* (fifo-push seg* (make-seg (reverse rt*))))
                                    (flush)))
                              (set! rt* '())
                              (constrain-width))
                       (begin (flush) (newline))))))))

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
  (mlet ((depth 0) (separate? #f) (right-bracket-count 0))
    (define (end-bracketed!)
      (range-for-each (lambda (i) (layout-group-end l)) right-bracket-count)
      (set! right-bracket-count 0)
      (set! separate? #t))
    (define (separate)
      (when (< 0 right-bracket-count) (end-bracketed!))
      (when separate? (layout-space^newline l))
      (set! separate? #t))
    (define (place t t.d) (layout-place l t t.d))
    (make-writer
      (lambda (text text.display datum) (separate) (place text text.display))
      (lambda (text text.display datum) (separate) (place text text.display) (set! separate? #f))
      (lambda (text text.display)       (separate) (place text text.display))
      (lambda (text text.display datum)
        (separate)
        (place text text.display)
        (set! separate? #f)
        (layout-group-begin l 0)
        (set! depth (+ depth 1)))
      (lambda (text text.display)
        (place text text.display)
        (set! depth (- depth 1))
        (set! right-bracket-count (+ right-bracket-count 1))
        (unless (< 0 depth) (end-bracketed!))))))

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
#;(let ((example
        '(() (0) 1 #('2 three "four" #(100 101 102 103 104 105 106 107 108 109 110 111) #"fiveeee") #(6 7 7 7) #t #f . 10))
      (example-writer/sgr (lambda (l)
                            (writer:layout/sgr l #"\e[0m" #"\e[33;5m" #"\e[31;5m" #"\e[32m"
                                               (lambda (datum)
                                                 (cond ((symbol? datum)  #"\e[34m")
                                                       ((number? datum)  #"\e[35m")
                                                       ((boolean? datum) #"\e[33m")
                                                       ((null? datum)    #"\e[32m")
                                                       (else             #"\e[36m"))))))
      (verbose-notate (make-notate '((abbreviate-reader-macro? . #t)
                                     (abbreviate-pair? . #f)
                                     (bracket . #"[")
                                     (length-prefix? . #t)
                                     (bytevector-numeric? . #t)))))
  (notate (writer:layout (layout:single-line (printer:port standard-output-port))) example)
  (oport-write-byte standard-output-port 10)
  (notate (example-writer/sgr (layout:single-line (printer:port standard-output-port))) example)
  (oport-write-byte standard-output-port 10)
  (verbose-notate (writer:layout (layout:single-line (printer:port standard-output-port))) example)
  (oport-write-byte standard-output-port 10)
  (verbose-notate (example-writer/sgr (layout:single-line (printer:port standard-output-port))) example)
  (oport-write-byte standard-output-port 10)
  (oport-write-byte standard-output-port 10)
  (let ((width 80))
    (notate (writer:layout (layout:compact (printer:port standard-output-port) width)) example)
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    (notate (example-writer/sgr (layout:compact (printer:port standard-output-port) width)) example)
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    (verbose-notate (writer:layout (layout:compact (printer:port standard-output-port) width)) example)
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    (verbose-notate (example-writer/sgr (layout:compact (printer:port standard-output-port) width)) example)
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    (let* ((l      (layout:compact (printer:port standard-output-port) width))
           (place  (lambda (t) (layout-place l t t)))
           (gbegin (lambda () (layout-group-begin l 0)))
           (gend   (lambda () (layout-group-end l)))
           (s^nl   (lambda () (layout-space^newline l)))
           (nl     (lambda () (layout-newline l))))
      (gbegin)
      (place #"(")
      (gbegin)
      (place #"one")
      ;(s^nl)
      (nl)
      (place #"two")
      (s^nl)
      ;(nl)
      (place #"three")
      (s^nl)
      (place #"four")
      (place #")")
      (gend)
      (s^nl)
      (place #"five")
      (s^nl)
      (place #"six")
      (gend))
    (oport-write-byte standard-output-port 10)
    (oport-write-byte standard-output-port 10)
    ))
