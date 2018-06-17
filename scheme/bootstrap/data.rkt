#lang racket/base
(provide
  atom?
  mvector mvector? mvector-v
  continuation continuation? continuation-k
  closure closure? closure-variadic? closure-param* closure-body closure-env
  primitive-ops
  )

(require
  "syntax.rkt"
  "../type.rkt"
  racket/vector
  )

;; TODO: support user-defined type definitions.

(define (atom? datum) (or (boolean? datum) (number? datum) (char? datum)
                          (string? datum) (symbol? datum) (null? datum)))

(define-type continuation continuation? continuation-k)
(define-type closure closure?
  closure-variadic? closure-param* closure-body closure-env)

(define-type (mvector _mvector) mvector? mvector-id mvector-v)
(define mvector (let ((id 0)) (lambda (v) (set! id (+ 1 id)) (_mvector id v))))
(define (make-mvector i k) (mvector (make-vector i k)))
(define (mvector-set! mv i v) (vector-set! (mvector-v mv) i v))
(define (mvector->vector mv) (vector-copy (mvector-v mv)))

(define (eqv?? d) (or (atom? d) (mvector? d)))
(define (identifier? d) (or (symbol? d) (labeled-name? d)))

(define primitive-ops
  `((eqv?     (,eqv?? ,eqv??)       ,eqv?)
    (syntax=? (,environment? #f #f) ,syntax=?)

    (procedure? (#f) ,closure?)
    (mvector?   (#f) ,mvector?)
    (vector?    (#f) ,vector?)
    (pair?      (#f) ,pair?)
    (null?      (#f) ,null?)
    (string?    (#f) ,string?)
    (char?      (#f) ,char?)
    (number?    (#f) ,number?)
    (integer?   (#f) ,integer?)
    (symbol?    (#f) ,symbol?)
    (boolean?   (#f) ,boolean?)
    (not        (#f) ,not)

    (char->integer  (,char?)    ,char->integer)
    (integer->char  (,integer?) ,integer->char)
    (string->vector (,string?)  ,(lambda (s) (list->vector (string->list s))))
    (vector->string (,vector?)  ,(lambda (v) (list->string (vector->list v))))
    (string->symbol (,string?)  ,string->symbol)
    (symbol->string (,symbol?)  ,symbol->string)

    (cons (#f #f)  ,cons)
    (car  (,pair?) ,car)
    (cdr  (,pair?) ,cdr)

    (vector-ref    (,vector? ,integer?) ,vector-ref)
    (vector-length (,vector?)           ,vector-length)

    (make-mvector    (,integer? #f)           ,make-mvector)
    (mvector-set!    (,mvector? ,integer? #f) ,mvector-set!)
    (mvector->vector (,mvector?)              ,mvector->vector)

    (=  (,number? ,number?) ,=)
    (<= (,number? ,number?) ,<=)
    (<  (,number? ,number?) ,<)
    (+  (,number? ,number?) ,+)
    (*  (,number? ,number?) ,*)

    ;bitwise-and
    ;bitwise-ior
    ;bitwise-xor
    ;bitwise-not
    ;bitwise-bit-set?
    ;bitwise-bit-field
    ;arithmetic-shift
    ;integer-length

    ;round
    ;quotient
    ;remainder

    ;; These could be derived instead.
    (-  (,number? ,number?) ,-)
    (/  (,number? ,number?) ,/)

    (append (,list? #f) ,append)
    (list->vector (,list?) ,list->vector)
    ))
