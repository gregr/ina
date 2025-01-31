(define env.test (env-conjoin/match env.large))

(define (test stx)
  (newline)
  (displayln "INPUT:")
  (pretty-write stx)
  (let ((E (parse-expression env.test stx)))
    (displayln "PARSED:")
    (pretty-write E)
    (displayln "PRETTY PARSED:")
    (pretty-write (E-pretty E))
    (displayln "EQUIVALENT RACKET CODE:")
    (pretty-write (E-compile-rkt E '()))
    (displayln "VALUE:")
    (pretty-write (E-eval E))))

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

;(block
; (define x 5)
; (block
;  (define-syntax m
;    (syntax-rules ()
;      ((_ def) (begin
;                 (define x 7)
;                 def))))
;  (m (define y (lambda () x)))
;  (define x 6)
;  (y)))
(test '(let ()
         (define x 5)
         (let ()
           (define-syntax (m stx)
             (match (syntax->list stx)
               ((list _ def)
                (quasiquote-syntax
                 (begin
                   (define x 7)
                   #,def)))))
           (m (define y (lambda () x)))
           (define x 6)
           (y))))
;==> 6
;EQUIVALENT RACKET CODE:
;((case-lambda
;   (()
;    (letrec ((x.0 (quote 5)))
;      ((case-lambda
;         (()
;          (letrec ((x.1 (quote 7)) (y.2 (case-lambda (() x.3))) (x.3 (quote 6)))
;            (y.2)))))))))



;(block
; (define x 4)
; (define-syntax intro-ref
;   (syntax-rules ()
;     ((_ v) (define v x))))
; (block
;  (define x 5)
;  (intro-ref y)))
(test '(let ()
         (define x 4)
         (define-syntax (intro-ref stx)
           (match (syntax->list stx)
             ((list _ v) (quasiquote-syntax (define #,v x)))))
         (let ()
           (define x 5)
           (intro-ref y)
           y)))
;==> 4
;EQUIVALENT RACKET CODE:
;((case-lambda
;   (()
;    (letrec ((x.0 (quote 4)))
;      ((case-lambda (() (letrec ((x.1 (quote 5)) (y.2 x.0)) y.2))))))))


;(let ((x 5))
;  (let-syntax ((let-m (syntax-rules () ((_ m b)
;                                        (let-syntax ((m (syntax-rules () ((_) x))))
;                                          b)))))
;    (let ((x 4))
;      (let-m m (m)))))
(test '(let ((x 5))
         (define-syntax (let-m stx)
           (match (syntax->list stx)
             ((list _ m b)
              (quasiquote-syntax
               (let ()
                 (define-syntax (#,m stx)
                   (quote-syntax x))
                 #,b)))))
         (let ((x 4))
           (let-m m (m)))))
;==> 5
;EQUIVALENT RACKET CODE:
;((case-lambda
;   ((x.0) ((case-lambda ((x.1) ((case-lambda (() x.0))))) (quote 4))))
; (quote 5))

(test '(let ()
         (begin-meta
          (splicing-let ((x 5))
            (define foo (quote-syntax x))))
         (define-syntax (m stx)
           (match (syntax->list stx)
             ((list _)
              (quasiquote-syntax
               (let ((#,foo 7))
                 #,foo)))))
         (splicing-let ((x 6))
           (m))))
;==> 7
;EQUIVALENT RACKET CODE:
;((case-lambda
;   (()
;    (letrec ((x.0
;              (call-with-values
;               (lambda () ((quote #<procedure:values>)))
;               (case-lambda (_.1 (quote 6))))))
;      ((case-lambda ((x.2) x.2)) (quote 7))))))

(test '(let ()
         (begin-meta
          (splicing-let ((x 5))
            (define foo (quote-syntax x))))
         (define-syntax (m stx)
           (match (syntax->list stx)
             ((list _ arg)
              (quasiquote-syntax
               (let ((#,arg 7))
                 #,arg)))))
         (splicing-let ((x 6))
           (m foo))))
;==> 7
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   (letrec ((x.0
;             (call-with-values
;              (lambda () ((quote #<procedure:values>)))
;              (case-lambda (_.1 (quote 6))))))
;     ((case-lambda ((foo.2) foo.2)) (quote 7))))))

(test '(let ()
         (begin-meta
          (splicing-let ((x 5))
            (define foo (quote-syntax x))))
         (define-syntax (m stx)
           (match (syntax->list stx)
             ((list _ arg)
              (quasiquote-syntax #,arg))))
         (splicing-let ((x 6))
           (m foo))))
;==> x
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   (letrec ((x.0
;             (call-with-values
;              (lambda () ((quote #<procedure:values>)))
;              (case-lambda (_.1 (quote 6))))))
;     (quote x)))))

;; unbound identifier
;(test '(let ()
;         (begin-meta
;          (splicing-let ((x 5))
;            (define foo (quote-syntax x))))
;         (define-syntax (m stx) foo)
;         (splicing-let ((x 6))
;           (m))))

(test '(let ()
         (begin-meta
          (splicing-let ((x 5))
            (define foo (quote-syntax x))))
         (splicing-let ((x 6))
           (define-syntax (m stx) foo))
         (splicing-let ((x 7))
           (m))))
;==> 6
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (()
;   (letrec ((x.0
;             (call-with-values
;              (lambda () ((quote #<procedure:values>)))
;              (case-lambda (_.2 (quote 6)))))
;            (x.1 (quote 7)))
;     x.0))))

(test '(let ()
         (splicing-let ((x 5))
           (define-vocabulary-value foo 'test (quote-syntax x)))
         (splicing-let ((x 6))
           (define-syntax (m stx)
             (lambda (env)
               (env-vocabulary-ref env (quote-syntax foo) 'test))))
         (splicing-let ((x 7))
           (m))))
;==> 6
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.1))))

(test '(let ()
         (splicing-let ((x 5))
           (define-vocabulary-value foo
             'env (current-environment)
             'stx (quote-syntax x)))
         (splicing-let ((x 6))
           (define-vocabulary-value m
             'expression-operator
             (lambda (env stx)
               (parse-expression
                (env-vocabulary-ref env (quote-syntax foo) 'env)
                (env-vocabulary-ref env (quote-syntax foo) 'stx)))))
         (splicing-let ((x 7))
           (m))))
;==> 5
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.0))))

(test '(let ()
         (splicing-let ((x 5))
           (define-vocabulary-value foo
             'env (current-environment)
             'stx (quote-syntax x)))
         (splicing-let ((x 6))
           (define-vocabulary-value m
             'expression-operator
             (lambda (env stx)
               (match (syntax->list stx)
                 ((list _ arg)
                  (parse-expression
                   (env-vocabulary-ref env arg 'env)
                   (env-vocabulary-ref env arg 'stx)))))))
         (splicing-let ((x 7))
           (m foo))))
;==> 5
;EQUIVALENT RACKET CODE:
;((case-lambda
;  (() (letrec ((x.0 (quote 5)) (x.1 (quote 6)) (x.2 (quote 7))) x.0))))

;; unbound identifier
;(test '(let ((x 44))
;         (define-syntax (m stx)
;           (define-syntax (m2 stx)
;             (quote-syntax (quote-syntax x)))
;           (quasiquote-syntax (let ((x 55)) #,(m2))))
;         (m)))

(test '(let ((x 44))
         (define-syntax (m stx)
           (define-syntax (m2 stx)
             (quote-syntax (quote x)))
           (quasiquote-syntax (let ((x 55)) #,(m2))))
         (m)))
;==> 55
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.1)) (quote 55)))) (quote 44))

(test '(let ((x 44))
         (define-syntax (m stx)
           (define (m2) (quote-syntax x))
           (quasiquote-syntax (let ((x 55)) #,(m2))))
         (m)))
;==> 55
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.1)) (quote 55)))) (quote 44))

(test '(let ((x 44))
         (define-syntax (m3 stx)
           (define-syntax (m4 stx)
             (quote-syntax (quote-syntax x)))
           (quasiquote-syntax (let ((#,(m4) 55)) x)))
         (m3)))
;==> 44
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.0)) (quote 55)))) (quote 44))

(test '(let ((x 44))
         (define-syntax (m3 stx)
           (define-syntax (m4 stx)
             (quote-syntax (quote x)))
           (quasiquote-syntax (let ((#,(m4) 55)) x)))
         (m3)))
;==> 55
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.1)) (quote 55)))) (quote 44))

(test '(let ((x 44))
         (define-syntax (m3 stx)
           (define (m4) (quote-syntax x))
           (quasiquote-syntax (let ((#,(m4) 55)) x)))
         (m3)))
;==> 55
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.1)) (quote 55)))) (quote 44))

(test '(let ((x 88))
         (define-syntax (m stx)
           (match (syntax->list stx)
             ((list _ a)
              (quasiquote-syntax
                (begin
                  (define #,a 77)
                  x)))))
         (m x)))
;==> 77
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) (letrec ((x.1 (quote 77))) x.1))) (quote 88))

(test '(let ((x 88))
         (define-syntax (m stx)
           (match (syntax->list stx)
             ((list _ a)
              (quasiquote-syntax
                (let ((#,a 77))
                  x)))))
         (m x)))
;==> 88
;EQUIVALENT RACKET CODE:
;((case-lambda ((x.0) ((case-lambda ((x.1) x.0)) (quote 77)))) (quote 88))
