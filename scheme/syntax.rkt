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

  syntax/metadata
  syntax-metadata

  syntax-mark
  syntax-rename

  mark-fresh
  renaming-fresh
  )

(module
  utilities racket/base
  (provide (all-defined-out))

  (require
    (for-syntax racket/base)
    racket/vector
    "record.rkt"
    )

  (define mark-fresh
    (let ((mark 0)) (lambda () (set! mark (+ 1 mark)) mark)))
  (define (mark? hdatum) (number? hdatum))

  (define label-fresh
    (let ((label 0)) (lambda () (set! label (+ 1 label)) label)))
  (define (renaming-fresh symbol marks) (vector symbol marks (label-fresh)))
  (define (renaming? hd) (vector? hd))
  (define (renaming-symbol r) (vector-ref r 0))
  (define (renaming-marks r) (vector-ref r 1))
  (define (renaming-label r) (vector-ref r 2))

  (define (hygiene-cons h h*)
    (cond ((null? h*) (list h))
          ((and (mark? h) (eqv? h (car h*))) (cdr h*))
          (else (cons h h*))))
  (define (hygiene-append h1 h2)
    (if (null? h1) h2
      (hygiene-cons (car h1) (hygiene-append (cdr h1) h2))))

  (define-record
    syntax-new syntax?
    syntax-datum (syntax-hygiene syntax-hygiene-set) syntax-metadata)

  (define (syntax/metadata datum metadata) (syntax-new datum '() metadata))

  (define (racket-syntax-metadata stx)
    (vector 'racket-source-info
            (syntax-source-module stx)
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx)
            (syntax-span stx)
            (syntax-position stx)))

  (define (syntax-new/racket datum stx)
    (syntax/metadata datum (racket-syntax-metadata stx)))

  (define (syntax-type? type? stx)
    (and (syntax? stx) (type? (syntax-datum stx))))
  (define (syntax-pair? stx) (syntax-type? pair? stx))
  (define (syntax-vector? stx) (syntax-type? vector? stx))
  (define (syntax-null? stx) (syntax-type? null? stx))
  (define (syntax-boolean? stx) (syntax-type? boolean? stx))
  (define (syntax-number? stx) (syntax-type? number? stx))
  (define (syntax-string? stx) (syntax-type? string? stx))
  (define (syntax-char? stx) (syntax-type? char? stx))

  ;; TODO: propagate hygiene.
  (define (syntax-pair-access access stx)
    (if (syntax-pair? stx) (access (syntax-datum stx))
      (error "datum is not a syntax pair:" stx)))
  (define (syntax-car stx) (syntax-pair-access car stx))
  (define (syntax-cdr stx) (syntax-pair-access cdr stx))
  (define (syntax-vector-ref stx idx)
    (if (syntax-vector? stx) (vector-ref (syntax-datum stx) idx)
      (error "datum is not a syntax vector:" stx)))

  (define (syntax->list stx)
    (cond ((syntax-null? stx) '())
          ((syntax-pair? stx) (cons (syntax-car stx)
                                    (syntax->list (syntax-cdr stx))))
          (else (error "datum is not a syntax list:" stx))))

  (define (syntax->datum stx)
    (define datum (syntax-datum stx))
    (cond ((pair? datum) (cons (syntax->datum (car datum))
                               (syntax->datum (cdr datum))))
          ((vector? datum) (vector-map syntax->datum datum))
          (else datum)))

  (define (datum->syntax stx datum)
    (syntax-hygiene-append datum (syntax-hygiene stx)))

  (define (syntax-hygiene-append datum hygiene)
    (if (syntax? datum)
      (syntax-hygiene-set
        datum (hygiene-append hygiene (syntax-hygiene datum)))
      (syntax-new datum hygiene #f)))

  (define (identifier? stx) (syntax-type? symbol? stx))

  (define (syntax-hygiene-cons stx h)
    (syntax-hygiene-set stx (hygiene-cons h (syntax-hygiene stx))))

  (define (syntax-mark stx mark) (syntax-hygiene-cons stx mark))
  (define (syntax-rename stx renaming) (syntax-hygiene-cons stx renaming))

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
