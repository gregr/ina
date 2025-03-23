(let* ((standard-output-port (current-output-port))
       (out standard-output-port)
       ;(out (thread-safe-oport standard-output-port))
       ;(out (buffered-oport standard-output-port))
       ;(out (buffered-oport (thread-safe-oport standard-output-port)))
       ;(out (buffered-oport/buffer-size standard-output-port 100000))
       ;(out (buffered-oport/buffer-size (thread-safe-oport standard-output-port) 100000))
       (example
         (append (list (bytevector->string (bytevector 0 15 16 31 127
                                                       ;#x03BB
                                                       #b11001110 #b10111011
                                                       ;159
                                                       #b11000010 #b10011111
                                                       ;8192
                                                       #b11100010 #b10000000 #b10000000
                                                       ;8233
                                                       #b11100010 #b10000000 #b10101001))
                       (string->symbol "ze\ro") (string->symbol "@zero") (string->symbol "ze#ro"))
                 '(() (0) 1 #('2 three "four" "\fou\r" #(100 101 102 103 104 105 106 107 108 109 110 111) #"\fi\ve" #"fiveeee") #(6 7 7 7) #t #f . 10)))
       (example-writer/sgr (lambda (l)
                             (writer:layout/sgr l #"\e[33;5m" #"\e[31;5m" #"\e[32m"
                                                (lambda (datum)
                                                  (cond ((symbol? datum)  #"\e[34m")
                                                        ((number? datum)  #"\e[35m")
                                                        ((boolean? datum) #"\e[33m")
                                                        ((null? datum)    #"\e[32m")
                                                        (else             #"\e[36m"))))))
       (example-printer/out  (lambda () (printer:port out)))
       (example-printer-fill (lambda (p) (printer-fill p 80 (bytevector-ref #"." 0) #f)))
       (example-printer      (lambda () (example-printer-fill (example-printer/out))))
       (example-printer/sgr
         (lambda ()
           (example-printer-fill
             (printer-sgr-add (printer-decorate/sgr (example-printer/out))
                              #"\e[47m"))))
       (verbose-notate (make-notate '((abbreviate-prefix? . #t)
                                      (abbreviate-pair? . #f)
                                      (bracket . #"[")
                                      (length-prefix? . #t)
                                      (bytevector-numeric? . #t)))))
  (notate example (writer:layout (layout:single-line (printer-truncate (example-printer) 78))))
  (oport-write-byte out 10)
  (oport-write-byte out 10)
  (notate example (example-writer/sgr (layout:single-line (printer-truncate (example-printer/sgr) 78))))
  (oport-write-byte out 10)
  (oport-write-byte out 10)
  (notate example (writer:layout (layout:single-line (printer-line-wrap (example-printer) 78))))
  (oport-write-byte out 10)
  (oport-write-byte out 10)
  (notate example (example-writer/sgr (layout:single-line (printer-line-wrap (example-printer/sgr) 78))))
  (oport-write-byte out 10)
  (oport-write-byte out 10)
  (notate example (writer:layout (layout:single-line (example-printer))))
  (oport-write-byte out 10)
  (notate example (example-writer/sgr (layout:single-line (example-printer/sgr))))
  (oport-write-byte out 10)
  (verbose-notate example (writer:layout (layout:single-line (example-printer))))
  (oport-write-byte out 10)
  (verbose-notate example (example-writer/sgr (layout:single-line (example-printer/sgr))))
  (oport-write-byte out 10)
  (oport-write-byte out 10)
  (let ((width 29))
    (notate example (writer:layout (layout:compact (printer-truncate (example-printer) width) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (notate example (example-writer/sgr (layout:compact (printer-truncate (example-printer/sgr) width) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (notate example (writer:layout (layout:compact (example-printer) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (notate example (example-writer/sgr (layout:compact (example-printer/sgr) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (verbose-notate example (writer:layout (layout:compact (example-printer) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (verbose-notate example (example-writer/sgr (layout:compact (example-printer/sgr) width)))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    (mdefine correlation* '())
    (define (correlate! text attr line col size)
      (set! correlation* (cons (vector text attr line col size) correlation*)))
    (let* ((l      (layout:pretty (printer-correlate-location (example-printer) correlate!) width))
           (place  (lambda (t) (layout-place l t #f)))
           (gbegin (lambda () (layout-group-begin l 0)))
           (gend   (lambda () (layout-group-end l)))
           (s^nl   (lambda () (layout-space^newline l)))
           (nl     (lambda () (layout-newline l))))
      (gbegin)
      (place #"(")
      (gbegin)
      (place #"one")
      (s^nl)
      ;(nl)
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
      ;(s^nl)
      (nl)
      (place #"six")
      (gend))
    (oport-write-byte out 10)
    (oport-write-byte out 10)
    ;(oport-close out)
    )
  (let* ((x->write (lambda (x)
                     (lambda (out)
                       (let ((p (printer:port out)))
                         (notate x (writer:layout (layout:single-line p)))
                         (printer-newline p)))))
         (out standard-output-port)
         ;(out (buffered-oport standard-output-port))
         ;(out (buffered-oport/buffer-size standard-output-port 100000))
         (example-write (lambda (x)
                          ((x->write x) out)
                          ;(sleep 1/10)
                          #t))
         (make-example-reader
           (lambda ()
             ((reader-track-line/start 0)
              (make-reader
                (lambda (loc text datum)          (example-write (vector 'atom:                 (list loc text datum))))
                (lambda (loc text type)           (example-write (vector 'prefix:               (list loc text type))))
                (lambda (loc text)                (example-write (vector 'dot:                  (list loc text))))
                (lambda (loc text shape type len) (example-write (vector 'left-bracket:         (list loc text shape type len))))
                (lambda (loc text shape)          (example-write (vector 'right-bracket:        (list loc text shape))))
                (lambda (loc text)                (example-write (vector 'datum-comment-prefix: (list loc text))))
                (lambda (loc text)                (example-write (vector 'comment:              (list loc text))))
                (lambda (loc c)                   (example-write (vector 'newline:              (list loc c))))
                (lambda (loc)                     (example-write (vector 'eof:                  (list loc))) #f)
                (lambda (loc text desc)           (example-write (vector 'error:                (list loc text desc))))))))
         (text.example (call/oport:bytevector (x->write example)))
         (text.another-example
           #"[#4{1 2} #vu8(50 51) #5vu8(100 110 120) #6\"abc\"
           #\"\\u03bb;\"
           #\"\\xce;\\xbb;\"
           \"\\xce;\\xbb;\"
           'quoted `qquoted ,unquoted ,@spliced #'squoted #`sqquoted #,sunquoted #,@sspliced
           ;; binary numbers:\n #b00 #b01 #b10 #b11 #;other-radixes: #xff #d256 #|fra#|ction|#s: |# .5 1/3 .~3 0.~1 #b.~1 #o.3~7 1e-3 #x1p3 #b1e11
           #|a
           multi-line
           block
           comment|#\"split
           by newline\"
           \"not split\
           by newline\"
           ;#!eof
           |after eof|]")
         (text.failure-example
           #"[#true #False #g @foo #4{1 2} #vu8(50 51) #5\"\" #6x\"abc\" #6\"abcdefg\"
           #\"\\u03gb;\\u03bb:;\\uffffffff;\\u;\"
           \"\\xce; \\xbb;\"
           #\"\\xcg;\\xbbb;\"
           ;; binary numbers:\n #b00 #b01 #b02 #;other-radixes: #xfg #d25a
           \"\\a\\b\\c unfinished")
           (text.here-bytevector-example
             ##test"##example"this here-bytevector contains literal text
             containing "embedded\ndouble quotes" etc.
             across multiple
             lines
             and can be conveniently embedded in another here-bytevector without escapes"example##"test##)
           (go (lambda (name text)
                 (example-write name)
                 (example-write text)
                 (example-write 'denotating:)
                 (denotate 0 (iport:bytevector text) (make-example-reader))
                 (oport-write-byte out 10)))
           (example-read-write
             (lambda (text)
               (example-write
                 ((read/reader:data/k (compose (reader-track-line/start 0) reader:data))
                  (iport:bytevector text)
                  (compose raise
                           (make-read-error-location-update
                             (lambda (loc)
                               (vector 'location
                                       (cons 'absolute-position (line-location-absolute-position loc))
                                       (cons 'line              (line-location-line              loc))
                                       (cons 'line-position     (line-location-line-position     loc))))))
                  mistake values)))))
              (go 'example: text.example)
              (go 'another-example: text.another-example)
              (go 'failure-example: text.failure-example)
              (go 'here-bytevector-example: text.here-bytevector-example)
              (example-read-write text.example)
              (oport-write-byte out 10)
              (example-read-write text.another-example)
              (oport-write-byte out 10)
              ;(example-read-write text.failure-example)
              (oport-write-byte out 10)
              (example-read-write text.here-bytevector-example)
              (oport-write-byte out 10)
              ;(oport-close out)
              ))
