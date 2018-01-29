#lang racket/base
(provide
  syntax
  syntax?
  ;quasisyntax
  ;unsyntax
  ;unsyntax-splicing

  ;syntax->datum
  ;datum->syntax

  ;identifier?
  ;free-identifier=?
  ;bound-identifier=?

  ;syntax-pair?
  ;syntax-vector?
  ;syntax-null?
  ;syntax-boolean?
  ;syntax-number?
  ;syntax-string?
  ;syntax-char?

  ;syntax-car
  ;syntax-cdr
  ;syntax-vector-ref

  syntax-new
  syntax-datum
  syntax-hygiene
  syntax-cursor
  syntax-metadata

  ;syntax-flipmark
  ;syntax-rename
  )

(module
  utilities1 racket/base
  (provide
    syntax?
    syntax-new
    syntax-datum
    syntax-hygiene
    syntax-cursor
    syntax-metadata
    racket-syntax->syntax
    )
  (require "record.rkt")

  (define-record
    syntax-new syntax?
    syntax-datum syntax-hygiene syntax-cursor syntax-metadata)

  (define (racket-syntax->syntax stx)
    (let ((metadata (vector 'racket
                            (syntax-source-module stx)
                            (syntax-source stx)
                            (syntax-line stx)
                            (syntax-position stx)
                            (syntax-span stx))))
      (syntax-new stx #f #f metadata))))

(module
  utilities2 racket/base
  (provide new-syntax)
  (require (for-syntax racket/base))
  (require (submod ".." utilities1))

  (define-syntax (new-syntax stx)
    (syntax-case stx () ((_ rstx) #'(racket-syntax->syntax #'rstx)))))

(require (for-syntax racket/base))
(require 'utilities1)
(require 'utilities2)

(define-syntax syntax (syntax-rules () ((_ s) (new-syntax s))))
