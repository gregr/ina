#lang racket/base
(provide
  racket-syntax
  racket-quasisyntax

  racket-syntax->syntax

  syntax
  quasisyntax

  syntax?
  syntax-unwrap
  syntax->datum
  datum->syntax

  identifier->label
  identifier?
  free-identifier=?
  bound-identifier=?
  identifier-rename

  syntax/metadata
  syntax-metadata

  syntax-rename/identifier
  syntax-rename/identifier*

  procedure->hygienic-syntax-transformer
  )

(require
  "syntax-new.rkt"
  )

(define (racket-syntax->syntax stx) (racket-syntax->syntax-new stx))

(define (syntax? d) (syntax-new? d))
(define (syntax->datum stx) (syntax-new->datum stx))
(define (datum->syntax stx d) (datum->syntax-new stx d))

(define (identifier? stx) (identifier-new? stx))
(define (free-identifier=? a b) (free-identifier-new=? a b))
(define (bound-identifier=? a b) (bound-identifier-new=? a b))

(define-syntax syntax (syntax-rules () ((_ s) (new-syntax s))))
(define-syntax quasisyntax (syntax-rules () ((_ s) (new-quasisyntax s))))
