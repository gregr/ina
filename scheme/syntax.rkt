#lang racket/base
(provide
  racket-syntax
  racket-quasisyntax

  syntax
  quasisyntax

  syntax?
  syntax->list
  syntax->datum
  datum->syntax

  identifier?
  ;free-identifier=?
  ;bound-identifier=?

  syntax-pair?
  syntax-vector?
  syntax-null?
  syntax-boolean?
  syntax-number?
  syntax-string?
  syntax-char?

  syntax-car
  syntax-cdr
  syntax-vector-ref

  syntax-new
  syntax-datum
  syntax-hygiene
  syntax-metadata

  ;syntax-flipmark
  ;syntax-rename
  )

(module
  utilities racket/base
  (provide (all-defined-out))

  (require
    (for-syntax racket/base)
    racket/vector
    "record.rkt"
    )

  (define-record
    syntax-new syntax? syntax-datum syntax-hygiene syntax-metadata)

  (define (racket-syntax-metadata stx)
    (vector 'racket-source-info
            (syntax-source-module stx)
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx)
            (syntax-span stx)
            (syntax-position stx)))

  (define (syntax-new/racket datum stx)
    (syntax-new datum '() (racket-syntax-metadata stx)))

  (define (syntax-type? type? stx)
    (or (type? stx)
        (and (syntax? stx) (syntax-type? type? (syntax-datum stx)))))
  (define (syntax-pair? stx) (syntax-type? pair? stx))
  (define (syntax-vector? stx) (syntax-type? vector? stx))
  (define (syntax-null? stx) (syntax-type? null? stx))
  (define (syntax-boolean? stx) (syntax-type? boolean? stx))
  (define (syntax-number? stx) (syntax-type? number? stx))
  (define (syntax-string? stx) (syntax-type? string? stx))
  (define (syntax-char? stx) (syntax-type? char? stx))

  (define (syntax-pair-access access stx)
    (cond ((pair? stx) (access stx))
          ((syntax? stx) (syntax-pair-access access (syntax-datum stx)))
          (else (error "datum is not a syntax pair:" stx))))
  (define (syntax-car stx) (syntax-pair-access car stx))
  (define (syntax-cdr stx) (syntax-pair-access cdr stx))

  (define (syntax-vector-ref stx idx)
    (cond ((vector? stx) (vector-ref stx idx))
          ((syntax? stx) (syntax-vector-ref (syntax-datum stx) idx))
          (else (error "datum is not a syntax vector:" stx))))

  (define (syntax->list stx)
    (cond ((syntax-null? stx) '())
          ((syntax-pair? stx)
           (cons (syntax-car stx) (syntax->list (syntax-cdr stx))))
          (else (error "datum is not a syntax list:" stx))))

  (define (syntax->datum stx)
    (cond ((syntax? stx) (syntax->datum (syntax-datum stx)))
          ((pair? stx) (cons (syntax->datum (car stx))
                             (syntax->datum (cdr stx))))
          ((vector? stx) (vector-map syntax->datum stx))
          (else stx)))

  (define (datum->syntax stx datum)
    (syntax-new datum (syntax-hygiene stx) #f))

  (define (identifier? stx) (syntax-type? symbol? stx))

  (define-syntax racket-syntax (syntax-rules () ((_ s) #'s)))
  (define-syntax racket-quasisyntax (syntax-rules () ((_ s) #`s)))


  (define-syntax (new-syntax stx)
    (syntax-case stx ()
      ((_ (a . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket `(,(new-syntax a) . ,(new-syntax d)) #'sdatum)))

      ((_ #(s ...))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket (vector (new-syntax s) ...) #'sdatum)))

      ((_ rstx) #'(syntax-new/racket (syntax-e #'rstx) #'rstx))))


  (define-syntax (new-quasisyntax stx)
    (syntax-case stx (unsyntax unsyntax-splicing)
      ((_ (unsyntax e)) #'e)

      ((_ ((unsyntax-splicing e) . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket (append (syntax->list e) (new-quasisyntax d))
                              #'sdatum)))

      ((_ (a . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket `(,(new-quasisyntax a) . ,(new-quasisyntax d))
                              #'sdatum)))

      ((_ #(s ...))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket
             (list->vector (syntax->list (new-quasisyntax (s ...))))
             #'sdatum)))

      ((_ s) #'(new-syntax s))))
  )

(require (for-syntax racket/base))
(require 'utilities)

(define-syntax syntax (syntax-rules () ((_ s) (new-syntax s))))
(define-syntax quasisyntax (syntax-rules () ((_ s) (new-quasisyntax s))))
