#lang racket/base
(require "nscheme.rkt"
         profile racket/function racket/include racket/list racket/match
         racket/port racket/pretty racket/string
         (for-syntax (except-in racket/base append string-ref string-length
                                string->list list->string))
         (rename-in (except-in racket/base append string-ref string-length
                               string->list list->string)
                    (read racket:read) (eof-object? racket:eof-object?)))
(module nscm:base racket
  (provide (all-defined-out))
  (require "nscheme.rkt" (for-syntax racket/list))
  (include "base.scm"))
(require 'nscm:base (for-syntax 'nscm:base))
(include "unicode.scm")
(include "grammar.scm")
(include "read.scm")
(include "write.scm")
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

(define (read*/string          s) (read*          (port:string:input s)))
(define (read*/annotate/string s) (read*/annotate (port:string:input s)))
(define (racket:read*/string   s) (call-with-input-string s racket:read*))

(define numbers "+1nan.0 +nan.0+i -inf.0i 0+1i 1+i -i 2-i 3-2/3i 4-inf.0i 5@5 #i1@1 1@+2 1@-2 .5 6. #e.75 #b1.1 5e-2 0 0.0 1 122 -3 4.0 500010000000.0 67.89 0.00001234")
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
    (test (list 'read/error: name)
      (read*/string        s)
      (racket:read*/string s)))
  '(    numbers/errors strings separation)
  (list numbers/errors strings separation))

(displayln "\nThese tests should pass:")
(for-each
  (lambda (name s)
    (test (list 'read: name)
      (read*/string        s)
      (racket:read*/string s)))
  '(    numbers symbols comments quotes)
  (list numbers symbols comments quotes))

(define categorized-data.normal
  (read*
    (port:string:input
      (string-join
        (list numbers numbers/errors symbols comments strings separation quotes)
        " "))))

(define categorized-data.annotated
  (read*/annotate
    (lambda (d p0 p1) `#s(annotated ,d ,p0 ,p1))
    (port:string:input
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
            (test (list 'read/annotate: i)
              (annotated-datum d.a)
              d.n)
            ;(test (list 'read/annotate.annotation: i)
            ;  d.a
            ;  '?)
            )
          (range (length categorized-data.normal))
          categorized-data.normal
          categorized-data.annotated)

(define fsys (filesystem '(".")))

;; TODO: read* via grammar is significantly slower at the moment:
;;   read*:              cpu time:   54 real time:   57 gc time:   3
;;   read*/experimental: cpu time: 1199 real time: 1206 gc time: 541
(define data
  (let* ((in (fsys 'open-input "read.scm"))
         (d (time (read* in))))
    (in 'close)
    d))
(define data.racket (time (call-with-input-file "read.scm" racket:read*)))

(test 'read.read.scm.length
  (length data)
  (length data.racket))

(for-each (lambda (i d d.racket)
            (test (list 'read.read.scm: i)
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

;(let loop ()
  ;(define datum (read (stdio 'in)))
  ;(when (not (eof-object? datum))
    ;(printf "~s\n" datum)
    ;(loop)))

;(define rin (port:string:input "\".@.(t457\"ok"))
;(define rin (port:string:input ".,@.\\(t457|\"o|k"))
;(define examples
  ;(append* (map (lambda (_)
                  ;(list "3238465928346598236459827364958762394856"
                        ;"457.03e51"
                        ;"-457.03e51"
                        ;"+457.03e-51"
                        ;"+457.03e+51"
                        ;"#o+457.03e51"
                        ;"#e#o+457.03e51"
                        ;"#o#e+457.03e51"
                        ;"+nan.0"
                        ;"-inf.0"
                        ;"+nan.0i"
                        ;"-inf.0i"
                        ;"-i"
                        ;"-57.03e51-23i"
                        ;"+57.03e51+34i"
                        ;"8@8"
                        ;"5/7"
                        ;"#i5/7"
                        ;)
                ;;(list "3238465928346598236459827364958762394856"
                       ;;"457.03e51 ywefjx"
                       ;;"-457.03e51 ywefjx"
                       ;;"+457.03e-51 ywefjx"
                       ;;"+457.03e+51 ywefjx"
                       ;;"#o+457.03e51 ywefjx"
                       ;;"#e#o+457.03e51 ywefjx"
                       ;;"#o#e+457.03e51 ywefjx"
                       ;;"+nan.0"
                       ;;"-inf.0"
                       ;;"+nan.0i"
                       ;;"-inf.0i"
                       ;;"-i"
                       ;;"-57.03e51-23i ywefjx"
                       ;;"+57.03e51+34i ywefjx"
                       ;;"8@8()"
                       ;;"5/7#t"
                       ;;"#i5/7#t"
                       ;;)
                ;)
                ;(range 500))))
;(define rins (map port:string:input examples))

;(void (profile (map string->number examples)))

;;(atom:true   rin 0)
;;(atom:true   rin 0)
;;(atom:false  rin 0)
;;(punctuation rin 0)
;;(atom:symbol rin 0)
;;(dot rin 0)
;;(atom:string rin 0)

;(displayln 'rx-matching)
;(define number-results
  ;(profile (map (lambda (rin) (atom:number rin 0)) rins)))
;(pretty-write
  ;(map (lambda (ex r)
         ;(cons ex (and r
                       ;(cons (substring ex 0 (mvector-ref
                                               ;r (- (mvector-length r) 2)))
                             ;(filter-not
                               ;not (map (lambda (seg)
                                          ;(define start (mvector-ref r (cdr seg)))
                                          ;(and start
                                               ;(cons (car seg) (substring ex start (mvector-ref r (+ (cdr seg) 1))))))
                                        ;(atom:number)))))))
       ;examples number-results))

(define (a^n n) (make-string n #\a))
(define (rx:a^n n) (rx `(seq ,@(make-list n '(? "a")) . ,(make-list n "a"))))
;(define (rx:a^n n) (rx `(seq . ,(make-list n "a"))))
;(define (rx:a^n n) (rx `(seq . ,(make-list n '(? "a")))))

(define (test-rx n)
  (displayln (a^n n))
  (define in (port:string:input (a^n n)))
  (define r (time (rx:a^n n)))
  ;(define r (time (rx '(* "a"))))
  (time (r in 0)))

;(test-rx 3000)

(test 'write.0
  (map (lambda (datum)
         (define out (port:string:output))
         (write datum out)
         (out 'string))
       `(#t #f () xyz |a b| |p q|\ r\|s|tu|
         ,(string-append "1 2\t3\n4"
                         (list->string (append '(5) (unicode->utf8 150))))
         0 0.0 1 122 -3 4.0 500010000000.0 67.89 0.00001234 #i3/10
         10/8 -2+3i +nan.0+i -inf.0 +inf.0i
         #(#t #f 1 2 3) (#t #f 1 2 3) (#t #f 1 2 . 3)))
  '("#t"
    "#f"
    "()"
    "xyz"
    "|a b|"
    "|p q r|\\||stu|"
    "\"1 2\\t3\\n4\\u5;\\u150;\""
    "0"
    "0.0"
    "1"
    "122"
    "-3"
    "4.0"
    "5.0001e11"
    "67.8900000000000005684341886080801486968994140625"                    ;; TODO: 67.89
    "1.23400000000000004368554129552393305857549421489238739013671875e-5"  ;; TODO: 1.234e-5
    "0.299999999999999988897769753748434595763683319091796875"             ;; TODO: 0.3
    "5/4"
    "-2+3i"
    "+nan.0+1.0i"
    "-inf.0"
    "0.0+inf.0i"
    "#(#t #f 1 2 3)"
    "(#t #f 1 2 3)"
    "(#t #f 1 2 . 3)"))

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
