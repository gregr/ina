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
  syntax->outer-datum

  label=?
  identifier->label
  identifier?
  free-identifier=?
  bound-identifier=?
  identifier->fresh-renaming

  syntax/metadata
  syntax-metadata

  syntax-mark
  syntax-rename
  syntax-rename*

  renaming?
  renaming-label
  )

(module
  utilities racket/base
  (provide (all-defined-out))

  (require
    "type.rkt"
    (for-syntax racket/base)
    racket/vector
    )

  (define mark-fresh
    (let ((mark 0)) (lambda () (set! mark (+ 1 mark)) mark)))
  (define (mark? hdatum) (number? hdatum))

  (define label-fresh
    (let ((label 0)) (lambda () (set! label (+ 1 label)) label)))
  (define (label=? a b) (eqv? a b))
  (define (renaming-fresh symbol marks) (vector symbol marks (label-fresh)))
  (define (renaming? hd) (vector? hd))
  (define (renaming-symbol r) (vector-ref r 0))
  (define (renaming-marks r) (vector-ref r 1))
  (define (renaming-label r) (vector-ref r 2))
  (define (identifier->fresh-renaming i)
    (when (not (identifier? i)) (error "cannot rename non-identifier:" i))
    (renaming-fresh (syntax->datum i) (syntax->mark* i)))

  (define hygiene-empty '())
  (define (hygiene-cons h h*)
    (cond ((null? h*) (list h))
          ((and (mark? h) (eqv? h (car h*))) (cdr h*))
          (else (cons h h*))))
  (define (hygiene-append h1 h2)
    (if (null? h1) h2
      (hygiene-cons (car h1) (hygiene-append (cdr h1) h2))))

  (define (hygiene->mark* h*) (filter mark? h*))

  (define (hygiene->label h* symbol)
    (cond ((null? h*) symbol)
          (else (define h (car h*))
                (if (and (renaming? h)
                         (eqv? (renaming-symbol h) symbol)
                         (equal? (renaming-marks h) (hygiene->mark* (cdr h*))))
                  (renaming-label h)
                  (hygiene->label (cdr h*) symbol)))))

  (define-type
    (syntax syntax-new (lambda (s) (list (syntax-datum s)))) syntax?
    syntax-datum (syntax-hygiene syntax-hygiene-set) syntax-metadata)

  (define (syntax/metadata datum metadata)
    (syntax-new datum hygiene-empty metadata))

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

  (define (syntax-hygiene-append datum hygiene)
    (if (syntax? datum)
      (syntax-hygiene-set
        datum (hygiene-append hygiene (syntax-hygiene datum)))
      (syntax-new datum hygiene #f)))

  (define (syntax-hygiene-cons stx h)
    (syntax-hygiene-set stx (hygiene-cons h (syntax-hygiene stx))))
  (define (syntax->mark* stx) (hygiene->mark* (syntax-hygiene stx)))

  (define (syntax-mark stx mark) (syntax-hygiene-cons stx mark))
  (define (syntax-rename stx renaming) (syntax-hygiene-cons stx renaming))
  (define (syntax-rename* stx renaming*)
    (foldl (lambda (r stx) (syntax-rename stx r)) stx renaming*))

  (define (syntax->outer-datum stx)
    (define datum (syntax-datum stx))
    (define h (syntax-hygiene stx))
    (cond ((pair? datum) (cons (syntax-hygiene-append (car datum) h)
                               (syntax-hygiene-append (cdr datum) h)))
          ((vector? datum)
           (vector-map (lambda (s) (syntax-hygiene-append s h)) datum))
          (else datum)))

  (define (syntax->list stx)
    (define d (syntax->outer-datum stx))
    (cond ((null? d) '())
          ((pair? d) (cons (car d) (syntax->list (cdr d))))
          (else (error "datum is not a syntax list:" stx))))

  (define (syntax->datum stx)
    (define (strip datum)
      (cond ((syntax? datum) (strip (syntax-datum datum)))
            ((pair? datum) (cons (strip (car datum)) (strip (cdr datum))))
            ((vector? datum) (vector-map strip datum))
            (else datum)))
    (strip (syntax-datum stx)))

  (define (datum->syntax stx datum)
    (if stx
      (syntax-hygiene-append datum (syntax-hygiene stx))
      (syntax/metadata datum #f)))

  (define (identifier? stx) (and (syntax? stx) (symbol? (syntax-datum stx))))

  (define (identifier->label i)
    (when (not (identifier? i))
      (error "cannot compute label of non-identifier:" i))
    (hygiene->label (syntax-hygiene i) (syntax-datum i)))

  (define (free-identifier=? a b)
    (label=? (identifier->label a) (identifier->label b)))

  (define (bound-identifier=? a b)
    (when (not (and (identifier? a) (identifier? b)))
      (error "cannot compare non-identifiers:" a b))
    (and (identifier? a) (identifier? b)
         (eqv? (syntax-datum a) (syntax-datum b))
         (equal? (syntax->mark* a) (syntax->mark* b))))

  (define-syntax racket-syntax (syntax-rules () ((_ s) #'s)))
  (define-syntax racket-quasisyntax (syntax-rules () ((_ s) #`s)))

  (define-syntax (new-syntax stx)
    (syntax-case stx ()
      ((_ (a . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket `(,(new-syntax a) . ,(new-syntax d))
                              (quote-syntax sdatum))))

      ((_ #(s ...))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket (vector (new-syntax s) ...)
                              (quote-syntax sdatum))))

      ((_ rstx) #'(syntax-new/racket 'rstx (quote-syntax rstx)))))

  (define-syntax (new-quasisyntax stx)
    (syntax-case stx (unsyntax unsyntax-splicing)
      ((_ (unsyntax e)) #'e)

      ((_ ((unsyntax-splicing e) . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket (append (syntax->list e) (new-quasisyntax d))
                              (quote-syntax sdatum))))

      ((_ (a . d))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket `(,(new-quasisyntax a) . ,(new-quasisyntax d))
                              (quote-syntax sdatum))))

      ((_ #(s ...))
       (with-syntax
         (((_ sdatum) stx))
         #'(syntax-new/racket
             (list->vector (syntax->list (new-quasisyntax (s ...))))
             (quote-syntax sdatum))))

      ((_ s) #'(new-syntax s)))))

(require 'utilities)

(define-syntax syntax (syntax-rules () ((_ s) (new-syntax s))))
(define-syntax quasisyntax (syntax-rules () ((_ s) (new-quasisyntax s))))
