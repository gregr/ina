#lang racket/base
(provide
  ;; TODO:
  )
(require
  "syntax-new.rkt"
  "type.rkt"
  racket/match
  racket/set
  )

(define-variant-type
  pat?
  (pat-exist pat-exist? pat-exist-ids pat-exist-p)
  (pat-any pat-any?)
  (pat-var pat-var? pat-var-id)
  (pat-literal pat-literal? pat-literal-datum)

  (pat-cons pat-cons? pat-cons-car pat-cons-cdr)
  (pat-segment
    pat-segment? pat-segment-min-length pat-segment-p pat-segment-cdr)
  (pat-vector pat-vector? pat-vector-lp)
  (pat-syntax pat-syntax? pat-syntax-vlp)

  (pat-and pat-and? pat-and-c1 pat-and-c2)
  (pat-or pat-or? pat-or-d1 pat-or-d2)
  (pat-not pat-not? pat-not-p)

  (pat-? pat-?? pat-?-predicate)
  (pat-app pat-app? pat-app-transformer pat-app-p)
  )

(define set-empty (set))

(define (bound-pattern-ids pat)
  (cond
    ((pat-exist? pat)
     (set-subtract (bound-pattern-ids (pat-exist-p pat)) (pat-exist-ids pat)))
    ((pat-var? pat) (set (pat-var-id pat)))
    ((pat-and? pat)     (set-union (bound-pattern-ids (pat-and-c1 pat))
                                   (bound-pattern-ids (pat-and-c2 pat))))
    ((pat-or? pat)      (set-union (bound-pattern-ids (pat-or-d1 pat))
                                   (bound-pattern-ids (pat-or-d2 pat))))
    ((pat-cons? pat)    (set-union (bound-pattern-ids (pat-cons-car pat))
                                   (bound-pattern-ids (pat-cons-cdr pat))))
    ((pat-segment? pat) (set-union (bound-pattern-ids (pat-segment-p pat))
                                   (bound-pattern-ids (pat-segment-cdr pat))))
    ((pat-vector? pat)  (bound-pattern-ids (pat-vector-lp pat)))
    ((pat-syntax? pat)  (bound-pattern-ids (pat-syntax-vlp pat)))
    ((pat-app? pat)     (bound-pattern-ids (pat-app-p pat)))
    (else set-empty)))

(define (simplify-pat pat)
  (cond
    ((pat-exist? pat)
     (pat-exist (pat-exist-ids pat) (simplify-pat (pat-exist-p pat))))
    ((pat-and? pat)
     (pat-and (simplify-pat (pat-and-c1 pat)) (simplify-pat (pat-and-c2 pat))))
    ((pat-or? pat)
     (pat-or (simplify-pat (pat-or-d1 pat)) (simplify-pat (pat-or-d2 pat))))
    ((pat-not? pat) (pat-not (simplify-pat (pat-not-p pat))))
    ((pat-cons? pat)
     (define pcar (simplify-pat (pat-cons-car pat)))
     (define pcdr (simplify-pat (pat-cons-cdr pat)))
     (if (and (pat-literal? pcar) (pat-literal? pcdr))
       (pat-literal (cons (pat-literal-datum pcar) (pat-literal-datum pcdr)))
       (pat-cons pcar pcdr)))
    ((pat-segment? pat) (pat-segment (pat-segment-min-length pat)
                                     (simplify-pat (pat-segment-p pat))
                                     (simplify-pat (pat-segment-cdr pat))))
    ((pat-vector? pat)
     (define lp (simplify-pat (pat-vector-lp pat)))
     (if (pat-literal? lp)
       (pat-literal (list->vector (pat-literal-datum lp)))
       (pat-vector lp)))
    ((pat-syntax? pat)
     (define vlp (simplify-pat (pat-syntax-vlp pat)))
     (if (pat-literal? vlp)
       (let ((datum (pat-literal-datum vlp)))
         (pat-literal
           (if (syntax-new? datum) datum (datum->syntax-new #f datum))))
       (pat-syntax vlp)))
    ((pat-app? pat)
     (pat-app (pat-app-transformer pat) (simplify-pat (pat-app-p pat))))
    (else pat)))

(define (syntax->pat stx)
  (define (non-null-atom? datum)
    (or (eq? #t datum)
        (not datum)
        (char? datum)
        (string? datum)
        (number? datum)))

  (define (k-or-more? id)
    (define (valid-k? k-chars)
      (define k (and (andmap char-numeric? k-chars)
                     (string->number (list->string k-chars))))
      (and (integer? k) (<= 0 k) k))
    (and (identifier? id)
         (match (string->list (symbol->string (syntax->datum id)))
           ((list #\. #\. k ...) #:when (valid-k? k) (valid-k? k))
           ((list #\_ #\_ k ...) #:when (valid-k? k) (valid-k? k))
           (_ #f))))

  (define-syntax define-quasi->pat
    (syntax-rules ()
      ((_ qq-syntax->pat quasiqu qu unqu unqu-splicing)
       (define (qq-syntax->pat stx)
         (syntax-case stx (list qu unqu unqu-splicing)
           ((unqu p) (syntax->pat #'p))
           (((unqu-splicing (list p (... ...))) . qq)
            (syntax->pat #'(list* p (... ...) (quasiqu qq))))
           (((unqu-splicing (qu (d (... ...)))) . qq)
            (syntax->pat #'((unqu-splicing (list (qu d) (... ...))) . qq)))
           (((unqu-splicing p) . qq)
            (identifier? #'p)
            (syntax->pat #'(list* p ___ (quasiqu qq))))
           ((a . d)
            (not (and (identifier? #'a)
                      (or (free-identifier=? #'unqu #'a)
                          (free-identifier=? #'unqu-splicing #'a))))
            (syntax->pat #'(list* (quasiqu a) (quasiqu d))))
           (#(p (... ...)) (pat-vector (qq-syntax->pat #'(p (... ...)))))
           (datum (syntax->pat #'(qu datum))))))))

  (define-quasi->pat
    qq-syntax->pat quasiquote quote unquote unquote-splicing)
  (define-quasi->pat
    qs-syntax->pat quasisyntax syntax unsyntax unsyntax-splicing)

  (syntax-case stx (var exist ___
                        and or not ? app
                        cons list list* vector
                        quote quasiquote syntax quasisyntax)
    (_ (and (identifier? stx) (free-identifier=? #'_ stx)) pat-any)
    (id (identifier? #'id) (pat-var #'id))
    ((var id) (identifier? #'id) (pat-var #'id))
    ((exist (id ...) pat ...)
     (andmap identifier? (syntax->list #'(id ...)))
     (pat-exist (list->set (syntax->list #'(id ...)))
                (syntax->pat #'(and pat ...))))

    ((list* p) (syntax->pat #'p))
    ((list* p0 ooo p ...)
     (and (identifier? #'ooo) (or (free-identifier=? #'ooo #'___)
                                  (free-identifier=? #'ooo #'(... ...))))
     (pat-segment 0 (syntax->pat #'p0) (syntax->pat #'(list* p ...))))
    ((list* p0 __k p ...)
     (k-or-more? #'__k)
     (pat-segment (k-or-more? #'__k) (syntax->pat #'p0)
                  (syntax->pat #'(list* p ...))))
    ((list* p0 p ...) (pat-cons (syntax->pat #'p0) (syntax->pat #'(list* p ...))))

    ((cons a d) (syntax->pat #'(list* a d)))
    ((list p ...) (syntax->pat #'(list* p ... '())))
    ((vector p ...) (pat-vector (syntax->pat #'(list p ...))))

    ((and p) (syntax->pat #'p))
    ((and p0 p ...) (pat-and (syntax->pat #'p0) (syntax->pat #'(and p ...))))
    ((or p) (syntax->pat #'p))
    ((or p0 p ...) (pat-or (syntax->pat #'p0) (syntax->pat #'(or p ...))))

    ((not p) (let* ((subpat (syntax->pat #'p))
                    (bound (bound-pattern-ids subpat)))
               (if (set-empty? bound)
                 (pat-not subpat)
                 (pat-not (pat-exist bound subpat)))))
    ((not p ...) (syntax->pat #'(not (or p ...))))

    ((? predicate) (pat-? #'predicate))
    ((? predicate p ...) (syntax->pat #'(and (? predicate) p ...)))
    ((app transformer p ...)
     (pat-app #'transformer (syntax->pat #'(and p ...))))

    ((quasiquote qq) (qq-syntax->pat #'qq))
    ((quasisyntax qs) (pat-syntax (qs-syntax->pat #'qs)))

    ((syntax datum) (pat-literal (racket-syntax->syntax-new #'datum)))
    ((quote datum) (pat-literal (syntax->datum #'datum)))
    (atom (non-null-atom? (syntax->datum #'atom))
          (pat-literal (syntax->datum #'atom)))))

;; TODO: named match for simple catamorphisms
