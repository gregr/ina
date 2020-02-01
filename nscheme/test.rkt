#lang racket/base
(require "nscheme.rkt"
         profile racket/function racket/include racket/list racket/match
         racket/pretty racket/string
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
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define (read* in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons datum (read* in))))
(define (read*/annotate annotate in)
  (let loop ()
    (define datum (read/annotate annotate in))
    (if (eof-object? datum) '() (cons datum (loop)))))

;(define (read*/experimental in)
  ;(define datum (read/experimental in))
  ;(if (eof-object? datum) '() (cons datum (read*/experimental in))))

(define (racket:read* in)
  (define datum (racket:read in))
  (if (racket:eof-object? datum) '() (cons datum (racket:read* in))))

(define numbers "+1nan.0 +nan.0+i -inf.0i 0+1i 1+i -i 2-i 5/0 -2/0 0/0 3-2/3i 4-inf.0i 5@5 #e5@5 #i1@1 #i1/0 #i-1/0 #i0/0 1@+2 1@-2 1@++5 .5 6. #e.75 #b1.1 5e-2")
(define symbols "|1+2i| \\3 |a b| c\\ d")
(define comments ";; foo\n x #;(1 2 3)y #| #| a b |# c d |# z")
(define strings "\"hello\\#d32;world\\#b1010;\\#x7e;\" \"abc\\u8232;\\u#d8233;def\"")
(define separation "#i#d1@1#i#xf#t#f test#(#t#t#f (ok . 123) 5)")
(define quotes "`(one ,two ,@(three 4 5) #(xxx ,'#(6 7)) #`(_ #,eight #,@splice _) #'nine . ten)")

(for-each
  (lambda (ds)
    (map (lambda (d) (if (procedure? d)
                       (printf "read error: ~s\n" (d))
                       (pretty-print (cond ((symbol? d) (list 'sym d))
                                           ((number? d) (list 'num d))
                                           (else        (list 'other d))))))
         ds))
  (map list
       (read* (port:string:input
                (string-join
                  (list numbers symbols comments strings separation quotes)
                  " ")))
       (read*/annotate
         vector (port:string:input
                  (string-join
                    (list numbers symbols comments strings separation quotes)
                    " ")))
       ;(read*/experimental
         ;(port:string:input
           ;(string-join
             ;(list numbers symbols comments strings separation quotes)
             ;" ")))
       ))

(define fsys (filesystem '(".")))

;; TODO: read*/experimental is significantly slower at the moment:
;;   read*:              cpu time:   54 real time:   57 gc time:   3
;;   read*/experimental: cpu time: 1199 real time: 1206 gc time: 541
(define data
  (let* ((in (fsys 'open-input "read.scm"))
         (d (time (read* in))))
    (in 'close)
    d))
;(define data/experimental
  ;(let* ((in (fsys 'open-input "read.scm"))
         ;(d (time (read*/experimental in))))
    ;(in 'close)
    ;d))
(define racket:data (time (call-with-input-file "read.scm" racket:read*)))

(newline)
(when (equal? data racket:data)
  (displayln "no diff"))
(unless (equal? data racket:data)
  (for-each
    (lambda (d/g d/r)
      (unless (equal? d/g d/r)
        (displayln "diff:")
        (pretty-print (if (procedure? d/g) (d/g) d/g))
        (newline)
        (displayln "vs:")
        (newline)
        (pretty-print d/r)))
    data ;(take data (length racket:data))
    racket:data)
  ;(displayln "diff:")
  ;(pretty-print data)
  ;(newline)
  ;(displayln "vs:")
  ;(newline)
  ;(pretty-print racket:data)
  )
;(when (equal? data/experimental racket:data)
  ;(displayln "no diff with experimental"))
;(unless (equal? data/experimental racket:data)
  ;(for-each
    ;(lambda (d/g d/r)
      ;(unless (equal? d/g d/r)
        ;(newline)
        ;(displayln "diff with experimental:")
        ;(pretty-print (if (procedure? d/g) (d/g) d/g))
        ;(newline)
        ;(displayln "vs:")
        ;(newline)
        ;(pretty-print d/r)))
    ;data/experimental ;(take data/experimental (length racket:data))
    ;racket:data))

;(let loop ()
  ;(define datum (read (stdio 'in)))
  ;(when (not (eof-object? datum))
    ;(printf "~s\n" datum)
    ;(loop)))

;(let loop ()
  ;(define datum (read/experimental (stdio 'in)))
  ;(when (not (eof-object? datum))
    ;(printf "~s\n" datum)
    ;(loop)))

;(define utf8-input '(124 133 8232 8233 82330 802330 8020330 80203030))
;(define utf8 (map unicode->utf8 utf8-input))
;utf8
;(map (lambda (bytes) (foldl (lambda (b u->u) (and (procedure? u->u) (u->u b))) utf8->unicode bytes))
     ;utf8)

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
;(pretty-print
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

(define (test n)
  (displayln (a^n n))
  (define in (port:string:input (a^n n)))
  (define r (time (rx:a^n n)))
  ;(define r (time (rx '(* "a"))))
  (time (r in 0)))

;(test 3000)
