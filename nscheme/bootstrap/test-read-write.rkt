#lang racket/base
(require
  "../platform/racket/nscheme.rkt" (for-syntax "../platform/racket/nscheme.rkt")
  profile racket/function racket/include racket/list racket/match
  racket/pretty racket/string
  (for-syntax (except-in racket/base case eqv? integer? rational? append string-ref string-length
                         string-append string->list list->string number->string let-values)
              racket/list)
  (rename-in racket/port
             (call-with-input-string  racket:call-with-input-string)
             (call-with-output-string racket:call-with-output-string))
  (rename-in (except-in racket/base case eqv? integer? rational? append string-ref string-length
                        string-append string->list list->string number->string let-values)
             (eof-object? racket:eof-object?)
             (read racket:read) (write racket:write)))
(module nscm:base racket
  (provide (all-defined-out))
  (require "../platform/racket/nscheme.rkt" (for-syntax racket/list))
  (include "../include/boot/string.scm")
  (include "../include/base/bytevector.scm")
  (include "../include/base/string.scm"))
(require 'nscm:base (for-syntax 'nscm:base))
(include "../include/unicode.scm")
(include "../include/grammar.scm")
(include "../include/read.scm")
(include "../include/write.scm")
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-syntax-rule (test name e.test e.expected)
  (begin (printf "Testing ~s:\n" name)
         (let ((expected e.expected) (answer e.test))
           (unless (equal? answer expected)
             (pretty-write 'e.test)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-write answer)
             (printf "  EXPECTED:\n")
             (pretty-write expected)))))

(define (read* in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons (if (procedure? datum)
                                      `#s(read-error ,(datum))
                                      datum)
                                    (read* in))))
(define (read*/annotate annotate in)
  (let loop ()
    (define datum (read/annotate annotate in))
    (if (eof-object? datum) '() (cons (if (procedure? datum)
                                        `#s(read-error ,(datum))
                                        datum)
                                      (loop)))))

(define (racket:read* in)
  (define datum (with-handlers ((exn:fail? (lambda (x) `#s(read-error ,(exn-message x)))))
                  (racket:read in)))
  (if (racket:eof-object? datum) '() (cons datum (racket:read* in))))

(define (read*/string          s) (read*          (string:port:input s)))
(define (read*/annotate/string s) (read*/annotate (string:port:input s)))
(define (racket:read*/string   s) (racket:call-with-input-string s racket:read*))

(define numbers "+1nan.0 +nan.0+i -inf.0i 0+1i 1+i -i 2-i 3-2/3i 4-inf.0i 5@5 #i1@1 1@+2 1@-2 .5 6. #e.75 #b1.1 5e-2 0 0.0 -0.0 #i10/3 1 122 -3 4.0 500010000000.0 67.89 0.00001234")
(define numbers/errors "5/0 -2/0 0/0 #i1/0 #i-1/0 #i0/0 #e5@5")
(define symbols "|1+2i| \\3 |a b| c\\ d 1@++5")
(define comments ";; foo\n x #;(1 2 3)y #| #| a b |# c d |# z")
(define strings "\"hello\\#d32;world\\#b1010;\\#x7e;\" \"abc\\u8232;\\u#d8233;def\"")
;; TODO: match Racket's behavior, which does not treat # as a separator?
(define separation "#i#d1@1#i#xf#t#f test#(#t#t#f (ok . 123) 5)")
(define quotes "`(one ,two ,@(three 4 5) #(xxx ,'#(6 7)) #`(_ #,eight #,@splice _) #'nine . ten)")

(displayln "These tests are known to diverge in behavior:")
(for-each
  (lambda (name s)
    (test (list 'read/error name)
      (read*/string        s)
      (racket:read*/string s)))
  '(    numbers/errors strings separation)
  (list numbers/errors strings separation))

(displayln "\nThese tests should pass:")
(for-each
  (lambda (name s)
    (define xs        (read*/string        s))
    (define xs.racket (racket:read*/string s))
    (for-each (lambda (x x.racket)
                (test (list 'read name x.racket)
                  x
                  x.racket))
              xs xs.racket))
  '(    numbers symbols comments quotes)
  (list numbers symbols comments quotes))

(define categorized-data.normal
  (read*
    (string:port:input
      (string-join
        (list numbers numbers/errors symbols comments strings separation quotes)
        " "))))

(define categorized-data.annotated
  (read*/annotate
    (lambda (d p0 p1) `#s(annotated ,d ,p0 ,p1))
    (string:port:input
      (string-join
        (list numbers numbers/errors symbols comments strings separation quotes)
        " "))))

(define (annotated-datum x)
  (match x
    (`#s(read-error ,message.a)    `#s(read-error ,(annotated-datum message.a)))
    (`#s(annotated ,datum ,p0 ,p1) (annotated-datum datum))
    (`(,a . ,d)                    (cons (annotated-datum a) (annotated-datum d)))
    ((vector xs ...)               (list->vector (map annotated-datum xs)))
    (_                             x)))

(for-each (lambda (i d.n d.a)
            (test (list 'read/annotate i)
              (annotated-datum d.a)
              d.n)
            ;(test (list 'read/annotate.annotation: i)
            ;  d.a
            ;  '?)
            )
          (range (length categorized-data.normal))
          categorized-data.normal
          categorized-data.annotated)

(require racket/runtime-path)
(define-runtime-path here ".")
(define path.include (path->string (build-path here "../include/")))

(define fsys (filesystem (list path.include)))

;; TODO: read* via grammar is significantly slower at the moment:
;;   read*:              cpu time:   54 real time:   57 gc time:   3
;;   read*/experimental: cpu time: 1199 real time: 1206 gc time: 541
(define data
  (let* ((in (fsys 'open-input "read.scm"))
         (d (time (read* in))))
    (in 'close)
    d))
(define data.racket (time (call-with-input-file (build-path path.include "read.scm") racket:read*)))

(test 'read.read.scm.length
  (length data)
  (length data.racket))

(for-each (lambda (i d d.racket)
            (test (list 'read 'read.scm i)
              d
              d.racket))
          (range (length data))
          data
          data.racket)

(for-each (lambda (s n)
            (test (list 'string->number s n)
              (string->number s)
              n))
          (list "3238465928346598236459827364958762394856"
                "457.03e50"
                "-457.03e50"
                "457.03e51"       ;; Racket's reader used to have a bug here
                "-457.03e51"
                "+457.03e-51"
                "+457.03e+51"
                "#o+457.03e51"
                "#e#o+457.03e51"
                "#o#e+457.03e51"
                "+nan.0"
                "-inf.0"
                "+nan.0i"
                "-inf.0i"
                "-i"
                "-57.03e51-23i"
                "+57.03e51+34i"
                "8@8"
                "5/7"
                "#i5/7"
                )
          (list 3238465928346598236459827364958762394856
                457.03e50
                -457.03e50
                457.03e51       ;; Racket's reader used to have a bug here
                -457.03e51
                +457.03e-51
                +457.03e+51
                #o+457.03e51
                #e#o+457.03e51
                #o#e+457.03e51
                +nan.0
                -inf.0
                +nan.0i
                -inf.0i
                -i
                -57.03e51-23i
                +57.03e51+34i
                8@8
                5/7
                #i5/7
                ))

(test 'rx.repetition
  (let ((n 3000))
    (define (a^n n) (make-string n #\a))
    (define (rx:a^n n) (rx '(* "a")))
    ;; These are expensive
    ;(define (rx:a^n n) (rx `(seq ,@(make-list n '(? "a")) . ,(make-list n "a"))))
    ;(define (rx:a^n n) (rx `(seq . ,(make-list n "a"))))
    ;(define (rx:a^n n) (rx `(seq . ,(make-list n '(? "a")))))
    (define (run-test n)
      (define in (string:port:input (a^n n)))
      (define r (time (rx:a^n n)))
      (time (r in 0)))
    (not (not (run-test n))))
  #t)

(for-each (lambda (x s)
            (define s.written
              (let ((out (string:port:output)))
                (write x out)
                (out 'string)))
            (test (list 'write x s)
              s.written
              s)
            (test (list 'write.read x s)
              (read (string:port:input s.written))
              x))
          `(#t
            #f
            ()
            xyz
            |a b|
            |p q|\ r\|s|tu|
            ,(string-append "1 2\t3\n4"
                            (utf8->string (u8*->bytevector (append '(5) (unicode->utf8 150)))))
            0
            0.0
            -0.0
            1
            122
            -3
            4.0
            5000100000.0
            50001000000.0
            67.89
            0.001234
            0.0001234
            #i3/10
            #i1/3
            #i10/3
            9.999999999999999e22
            1e23
            9.999999999999998e22
            9.999999999999997e22
            5.0001e10
            7e150
            0.000000000123456789123456789e250
            0.123456789123456789e250
            123456789123456789.123456789e250
            8e280
            10/8
            -2+3i
            +nan.0+i
            -inf.0
            +inf.0i
            #(#t #f 1 2 3)
            (#t #f 1 2 3)
            (#t #f 1 2 . 3))
          '("#t"
            "#f"
            "()"
            "xyz"
            "|a b|"
            "|p q r|\\||stu|"
            "\"1 2\\t3\\n4\\u5;\\u150;\""
            "0"
            "0.0"
            "-0.0"
            "1"
            "122"
            "-3"
            "4.0"
            "5000100000.0"
            "5.0001e10"
            "67.89"
            "0.001234"
            "1.234e-4"
            "0.3"
            "0.3333333333333333"
            "3.3333333333333335"
            "1e23"
            "1e23"
            "9.999999999999997e22"
            "9.999999999999997e22"
            "5.0001e10"
            "7e150"
            "1.2345678912345679e240"
            "1.2345678912345678e249"
            "1.234567891234568e267"
            "8e280"
            "5/4"
            "-2+3i"
            "+nan.0+1.0i"
            "-inf.0"
            "0.0+inf.0i"
            "#(#t #f 1 2 3)"
            "(#t #f 1 2 3)"
            "(#t #f 1 2 . 3)"))

(define data.formatted-writing
  '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16))
    (zero one two three four five six seven eight nine)
    #(zero one two three four five six seven eight nine)
    (zero one two three four five six seven eight nine . ten)
    (zero one two three four five six seven
          (zero one two three four five six seven eight nine . ten)
          eight nine . ten)
    (zero one two three
          #(zero one two three four five six seven eight nine)
          four five six seven eight nine
          (zero one two three four five six seven eight nine)
          ((zero one two three four five six seven eight nine)
           zero one two three four five six seven eight nine)
          ((zero one two three four five six seven eight nine)
           zero one two three four
           (zero one two three four five six seven eight nine)
           five six seven eight nine)
          ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16))
          #(#(zero one two three four five six seven eight nine)
            zero one two three four five six
            #(zero one two three four five six seven eight nine) seven eight nine))))

(let ((x data.formatted-writing)
      (s "((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16)) (zero one two three four five six seven eight nine) #(zero one two three four five six seven eight nine) (zero one two three four five six seven eight nine . ten) (zero one two three four five six seven (zero one two three four five six seven eight nine . ten) eight nine . ten) (zero one two three #(zero one two three four five six seven eight nine) four five six seven eight nine (zero one two three four five six seven eight nine) ((zero one two three four five six seven eight nine) zero one two three four five six seven eight nine) ((zero one two three four five six seven eight nine) zero one two three four (zero one two three four five six seven eight nine) five six seven eight nine) ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16)) #(#(zero one two three four five six seven eight nine) zero one two three four five six #(zero one two three four five six seven eight nine) seven eight nine)))"))
  (define s.written
    (let ((out (string:port:output)))
      (write x out)
      (out 'string)))
  ;(printf "formatted write:\n~a\n" s.written)
  (test (list 'write/default)
    s.written
    s)
  (test (list 'write/default.read)
    (read (string:port:input s.written))
    x))

(let ((x data.formatted-writing))
  (define s.written
    (let ((out (string:port:output)))
      ;(write x out 'pretty)
      (write x out '(pretty 80))
      (out 'string)))
  (define racket:s.written
    (string-trim
      (with-output-to-string (thunk (pretty-write data.formatted-writing)))))
  ;(printf "formatted write:\n~a\n" s.written)
  ;(printf "Racket's formatted write:\n~a\n" racket:s.written)
  (test (list 'write/pretty)
    s.written
    racket:s.written)
  (test (list 'write/pretty.read)
    (read (string:port:input s.written))
    x))

(let ((x data.formatted-writing)
      (s "((0\n  1\n  2\n  3\n  4\n  5\n  6\n  7\n  8\n  9\n  10\n  11\n  12\n  13\n  14\n  15\n  16)\n ((0\n   1\n   2\n   3\n   4\n   5\n   6)\n  7\n  8\n  9\n  10\n  11\n  12\n  (13\n   14\n   15\n   16))\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  eight\n  nine)\n #(zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  eight\n  nine\n  .\n  ten)\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  (zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine\n   .\n   ten)\n  eight\n  nine\n  .\n  ten)\n (zero\n  one\n  two\n  three\n  #(zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n  four\n  five\n  six\n  seven\n  eight\n  nine\n  (zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n  ((zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n  ((zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   zero\n   one\n   two\n   three\n   four\n   (zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   five\n   six\n   seven\n   eight\n   nine)\n  ((0\n    1\n    2\n    3\n    4\n    5\n    6)\n   7\n   8\n   9\n   10\n   11\n   12\n   (13\n    14\n    15\n    16))\n  #(#(zero\n      one\n      two\n      three\n      four\n      five\n      six\n      seven\n      eight\n      nine)\n    zero\n    one\n    two\n    three\n    four\n    five\n    six\n    #(zero\n      one\n      two\n      three\n      four\n      five\n      six\n      seven\n      eight\n      nine)\n    seven\n    eight\n    nine)))"))
  (define s.written
    (let ((out (string:port:output)))
      (write x out '(pretty 0))
      (out 'string)))
  ;(printf "formatted write:\n~a\n" s.written)
  (test (list 'write/pretty-thin)
    s.written
    s)
  (test (list 'write/pretty-thin.read)
    (read (string:port:input s.written))
    x))

(let ((x data.formatted-writing)
      (s "((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)\n ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16))\n (zero one two three four five six seven eight nine)\n #(zero one two three four five six seven eight nine)\n (zero one two three four five six seven eight nine . ten)\n (zero one two three four five six seven\n  (zero one two three four five six seven eight nine . ten) eight nine . ten)\n (zero one two three #(zero one two three four five six seven eight nine) four\n  five six seven eight nine (zero one two three four five six seven eight nine)\n  ((zero one two three four five six seven eight nine) zero one two three four\n   five six seven eight nine)\n  ((zero one two three four five six seven eight nine) zero one two three four\n   (zero one two three four five six seven eight nine) five six seven eight nine)\n  ((0 1 2 3 4 5 6) 7 8 9 10 11 12 (13 14 15 16))\n  #(#(zero one two three four five six seven eight nine) zero one two three four\n    five six #(zero one two three four five six seven eight nine) seven eight\n    nine)))"))
  (define s.written
    (let ((out (string:port:output)))
      ;(write x out 'bulky)
      (write x out '(bulky 80))
      (out 'string)))
  ;(printf "formatted write:\n~a\n" s.written)
  (test (list 'write/bulky)
    s.written
    s)
  (test (list 'write/bulky.read)
    (read (string:port:input s.written))
    x))

(let ((x data.formatted-writing)
      (s "((0\n  1\n  2\n  3\n  4\n  5\n  6\n  7\n  8\n  9\n  10\n  11\n  12\n  13\n  14\n  15\n  16)\n ((0\n   1\n   2\n   3\n   4\n   5\n   6)\n  7\n  8\n  9\n  10\n  11\n  12\n  (13\n   14\n   15\n   16))\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  eight\n  nine)\n #(zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  eight\n  nine\n  .\n  ten)\n (zero\n  one\n  two\n  three\n  four\n  five\n  six\n  seven\n  (zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine\n   .\n   ten)\n  eight\n  nine\n  .\n  ten)\n (zero\n  one\n  two\n  three\n  #(zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n  four\n  five\n  six\n  seven\n  eight\n  nine\n  (zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n  ((zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   zero\n   one\n   two\n   three\n   four\n   five\n   six\n   seven\n   eight\n   nine)\n  ((zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   zero\n   one\n   two\n   three\n   four\n   (zero\n    one\n    two\n    three\n    four\n    five\n    six\n    seven\n    eight\n    nine)\n   five\n   six\n   seven\n   eight\n   nine)\n  ((0\n    1\n    2\n    3\n    4\n    5\n    6)\n   7\n   8\n   9\n   10\n   11\n   12\n   (13\n    14\n    15\n    16))\n  #(#(zero\n      one\n      two\n      three\n      four\n      five\n      six\n      seven\n      eight\n      nine)\n    zero\n    one\n    two\n    three\n    four\n    five\n    six\n    #(zero\n      one\n      two\n      three\n      four\n      five\n      six\n      seven\n      eight\n      nine)\n    seven\n    eight\n    nine)))"))
  (define s.written
    (let ((out (string:port:output)))
      (write x out '(bulky 0))
      (out 'string)))
  ;(printf "formatted write:\n~a\n" s.written)
  (test (list 'write/bulky-thin)
    s.written
    s)
  (test (list 'write/bulky-thin.read)
    (read (string:port:input s.written))
    x))

(let* ((codepoints  '(124 133 8232 8233 82330 802330 8020330 80203030))
       (bytess.utf8 (map unicode->utf8 codepoints)))
  (test 'utf8.bytes
    bytess.utf8
    '((124)
      (194 133)
      (226 128 168)
      (226 128 169)
      (240 148 134 154)
      (243 131 184 154)
      (248 158 166 133 170)
      (252 132 177 188 180 150)))
  (test 'utf8.identity
    (map (lambda (bytes)
           (foldl (lambda (b u->u) (and (procedure? u->u) (u->u b)))
                  utf8->unicode bytes))
         bytess.utf8)
    codepoints))

(define benchmark
  (let ((out null:port:output))
    (time (for-each (lambda (_)
                      (for-each (lambda (x) (write x out))
                                '(4.0
                                  5000100000.0
                                  67.89
                                  0.001234
                                  1.234e-4
                                  0.3
                                  0.3333333333333333
                                  9.999999999999999e22
                                  1e23
                                  9.999999999999998e22
                                  9.999999999999997e22
                                  5.0001e10
                                  ;; these are much slower to write
                                  7e150
                                  0.000000000123456789123456789e250
                                  0.123456789123456789e250
                                  123456789123456789.123456789e250
                                  8e280)))
                    (range 100)))))

;(let loop ()
  ;(define datum (read (stdio 'in)))
  ;(when (not (eof-object? datum))
    ;(printf "       write: ~s\nracket:write: ~s\n"
            ;(let ((out (string:port:output)))
              ;(write datum out)
              ;(out 'string))
            ;(with-output-to-string
              ;(thunk (racket:write datum))))
    ;(loop)))
