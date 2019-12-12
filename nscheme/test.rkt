#lang racket
(require "nscheme.rkt"
         racket/pretty
         (rename-in racket/base
                    (read racket:read) (eof-object? racket:eof-object?)))
(include "read.scm")
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define (read* in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons datum (read* in))))

(define (racket:read* in)
  (define datum (racket:read in))
  (if (racket:eof-object? datum) '() (cons datum (racket:read* in))))

(define numbers "+1nan.0 +nan.0+i -inf.0i 0+1i 1+i -i 2-i 5/0 -2/0 0/0 3-2/3i 4-inf.0i 5@5 #e5@5 #i1@1 #i1/0 #i-1/0 #i0/0 1@+2 1@-2 1@++5 .5 6. #e.75")
(define symbols "|1+2i| \\3 |a b| c\\ d")
(define comments ";; foo\\\nbar\n x #;(1 2 3)y #| #| a b |# c d |# z")
(define strings "\"hello\\d32;world\\b1010;\\x7e;\" \"abc\\ud8232;def\"")
(define separation "#i#d1@1#i#xf#t#f test#(#t#t#f (ok . 123) 5)")
(define quotes "`(one ,two ,@(three 4 5) #(xxx ,'#(6 7)) #`(_ #,eight #,@splice _) #'nine . ten)")

(for-each (lambda (d) (if (procedure? d)
                        (printf "read error: ~s\n" (d))
                        (pretty-print (cond ((symbol? d) (list 'sym d))
                                            ((number? d) (list 'num d))
                                            (else        (list 'other d))))))
          (read* (port:string:input
                   (string-join
                     (list numbers symbols comments strings separation quotes)
                     " "))))

(define fsys (filesystem '(".")))
(define in (fsys 'open-input "read.scm"))
(define data (read* in))
(in 'close)
(define racket:data (call-with-input-file "read.scm" racket:read*))

(newline)
(when (equal? data racket:data)
  (displayln "no diff"))
(unless (equal? data racket:data)
  (displayln "diff:")
  (pretty-print data)
  (newline)
  (displayln "vs:")
  (newline)
  (pretty-print racket:data))
