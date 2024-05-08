#lang racket/base
(require
  "../platform/racket/nscheme.rkt" "include.rkt"
  racket/include racket/match racket/pretty racket/splicing
  (prefix-in rkt: racket/base))
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define env.test
  (let ((env-conjoin/match (E-eval (parse-expression env.large 'env-conjoin/match))))
    (env-conjoin/match env.large)))

(define test
  (let ((parse-expression (E-eval (parse-expression env.test 'parse-expression))))
    (lambda (stx)
      (newline)
      (displayln "INPUT:")
      (pretty-write stx)
      (with-pretty-panic
       (let ((E (parse-expression env.test stx)))
         (displayln "PARSED:")
         (pretty-write E)
         (displayln "PRETTY PARSED:")
         (pretty-write (E-pretty E))
         (displayln "EQUIVALENT RACKET CODE:")
         (pretty-write (E-compile-rkt E))
         (displayln "VALUE:")
         (pretty-write (E-eval E)))))))

;; Examples for other metaprogramming facilities
;(test '(current-environment))
;(test '(let ()
;         (define-vocabulary-value
;          thing 'expression
;          (lambda (env stx)
;            ($quote (vector 'thing-syntax: stx))))
;         (vector 'thing: thing)))
;(test '(let ()
;         (define-vocabulary-value
;          thing 'expression-operator
;          (lambda (env stx)
;            ($quote (vector 'thing-syntax: stx))))
;         (vector 'thing: (thing 1 2 3))))

;; Example of unhygienic expansion leading to an incorrect result.
;(test '(let ()
;         (define-vocabulary-value
;          example-or
;          'expression-operator
;          (lambda (env stx)
;            (parse-expression
;             env
;             (match (syntax->list stx)
;               ((list _ a b)
;                `(let ((tmp ,a))
;                   (if tmp
;                       tmp
;                       ,b)))
;               (x (raise-parse-error "bad syntax" x))))))
;         (let ((if #f)
;               (tmp 5))
;           (example-or if tmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hygienic macro expansion examples ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(begin
;  (define-syntax-rule (or a b)
;    (let ((tmp a))
;      (if tmp
;          tmp
;          b)))
;  (let ((if #f)
;        (tmp 5))
;    (or if tmp)))
;==>
;(let ((tmp_i if_u))
;  (if_i tmp_i
;        tmp_i
;        tmp_u))
(test '(let ()
         (define-syntax (example-or stx)
           (match (syntax->list stx)
             ((list _ a b)
              (quasiquote-syntax
               (let ((tmp #,a))
                 (if tmp
                     tmp
                     #,b))))))
         (let ((if #f)
               (tmp 5))
           (example-or if tmp))))
;==> 5
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   ((case-lambda
;     ((if.0 tmp.1) ((case-lambda ((tmp.2) (if tmp.2 tmp.2 tmp.1))) if.0)))
;    (quote #f)
;    (quote 5)))))
;; Same example using case-lambda instead of match:
;(test '(let ()
;         (define-syntax (example-or stx)
;           (apply
;            (case-lambda
;              ((_ a b)
;               (quasiquote-syntax
;                (let ((tmp #,a))
;                  (if tmp
;                      tmp
;                      #,b))))
;              (_ (raise-parse-error "bad syntax" stx)))
;            (syntax->list stx)))
;         (let ((if #f)
;               (tmp 5))
;           (example-or if tmp))))
;==> 5

;(begin
;  (define-syntax-rule (bar)
;    (define x 42))
;  (bar)
;  x)
(test '(let ((x 'not-42))
         (define-syntax (bar stx)
           (match (syntax->list stx)
             ((list _) '(define x 42))))
         (bar)
         x))
;==> not-42
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) (letrec ((x.1 (quote 42))) x.0))) (quote not-42))

;(begin
;  (define x 'bad)
;  (define-syntax-rule (bar stmt)
;    (begin
;      (define x 42)
;      stmt
;      x))
;  (let ()
;    (bar (define y 5))
;    y
;    x))
(test '(mlet ((cell #f))
         (define x 'bad)
         (define-syntax (bar stx)
           (match (syntax->list stx)
             ((list _ stmt)
              (quasiquote-syntax
               (begin
                 (define x 42)
                 #,stmt
                 (set! cell x))))))
         (let ()
           (bar (define y 5))
           (vector
            cell
            y
            x))))
;==> #(42 5 bad)
;EQUIVALENT RACKET CODE:
;((case-lambda
;  ((cell.0)
;   (letrec ((x.1 (quote bad)))
;     ((case-lambda
;       (()
;        (letrec ((x.2 (quote 42)) (y.3 (quote 5)))
;          (apply/values
;           (case-lambda
;            (X.4
;             ((quote #<procedure:vector>)
;              ((quote #<procedure:mvector-ref>) cell.0 (quote 0))
;              y.3
;              x.1)))
;           ((quote #<procedure:mvector-set!>) cell.0 (quote 0) x.2)))))))))
; ((quote #<procedure:make-mvector>) (quote 1) (quote #f)))

;(begin
;  (define x 'bad)
;  (define-syntax-rule (bar baz)
;    (begin
;      (define x 'good)
;      (define-syntax-rule (baz)
;        x)))
;  (let ()
;    (bar baz)
;    (baz)))
(test '(let ()
         (define x 'bad)
         (define-syntax (bar stx)
           (match (syntax->list stx)
             ((list _ baz)
              (quasiquote-syntax
               (begin
                 (define x 'good)
                 (define-syntax (#,baz stx)
                   (match (syntax->list stx)
                     ((list _) (quote-syntax x)))))))))
         (let ()
           (bar baz)
           (baz))))
;==> good
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   (letrec ((x.0 (quote bad)))
;     ((case-lambda (() (letrec ((x.1 (quote good))) x.1))))))))

;(begin
;  (define x 'bad)
;  (define y 'outer)
;  (let ()
;    (define-syntax-rule (bar arg)
;      (begin
;        (define y 'inner)
;        (define arg 'good)
;        x))
;    (bar x)))
(test '(let ()
         (define x 'bad)
         (define y 'outer)
         (let ()
           (define-syntax (bar stx)
             (match (syntax->list stx)
               ((list _ arg)
                (quasiquote-syntax
                 (begin
                   (define y 'inner)
                   (define #,arg 'good)
                   x)))))
           (bar x))))
;==> good
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   (letrec ((x.0 (quote bad)) (y.1 (quote outer)))
;     ((case-lambda
;       (() (letrec ((y.2 (quote inner)) (x.3 (quote good))) x.3))))))))
