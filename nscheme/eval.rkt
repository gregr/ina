#lang racket/base
(provide eval env:base s->ns ns->s plift)
(require "common.rkt" racket/bool racket/control)

(define (env-extend*/var env b*)
  (define (b->rib b)
    (let* ((cell (make-mvector 1 (cdr b)))
           (get (plift (lambda ()  (mvector-ref  cell 0))))
           (set (plift (lambda (v) (mvector-set! cell 0 v)))))
      (cons (car b) (list (cons ctx:var get) (cons ctx:set! set)))))
  (append (map b->rib b*) env))

;; Evaluation
(define (eval env form)
  (cond ((pair? form)
         (let* ((p (car form)) (a* (cdr form)) (proc (eval env p)))
           (if (and (string? p) (env-ref-prop env p ctx:op #f))
             ($apply proc (cons env a*)) ($apply proc (eval* env a*)))))
        ((string? form) ($apply (or (env-ref-prop env form ctx:var #f)
                                    (error '"unbound variable:" form)) '()))
        ((or (boolean? form) (number? form)) form)
        ((procedure? form)                   ($apply form (list env)))
        (#t                                  (error '"invalid syntax:" form))))
(define (eval* env form*) (map (lambda (f) (eval env f)) form*))

(define (@lambda env param . body)
  (let ((cenv (alist-remove* env (param-names param))))
    (lambda (a) (@body* (env-extend*/var cenv (param-bind param a)) body))))
(define (@quote env d) d)
(define (@if env c t f) (if (eval env c) (eval env t) (eval env f)))
(define (@reset env . body) (reset (@body* env body)))
(define (@shift env p . body)
  (shift k (apply @let env (list (list p (lambda (_) (plift k)))) body)))
(define (@set! env param arg)
  (for-each (lambda (b) ((or (env-ref-prop env (car b) ctx:set! #f)
                             (error '"identifier cannot be set!:" (car b)))
                         (list (cdr b))))
            (param-bind param (eval env arg))) #t)
(define (@letrec env b* . body)
  (bpair*?! b*)
  (define (k env) (begin (for-each (lambda (b) (apply @set! env b)) b*)
                         (apply @let env '() body)))
  (@let env (map (lambda (b) (list (car b) #t)) b*) (plift k)))
(define (@let/ env b* . body)
  ($apply (apply @lambda env (map car b*) body) (eval* env (map cadr b*))))
(define (@let/name env name b* . body)
  (define p (plift (lambda (env) (apply @lambda env (map car b*) body))))
  ($apply (@letrec env (list (list name p)) name) (eval* env (map cadr b*))))
(define (@let* env b* . body)
  (let loop ((b* b*) (env env))
    (if (null? b*) (@body* env body)
      (@let/ env (list (car b*)) (plift (lambda (env) (loop (cdr b*) env)))))))
(define (@let env . tail)
  (cond ((and (length>=? 2 tail) (string? (car tail)) (bpair*?! (cadr tail)))
         (apply @let/name env tail))
        (#t (bpair*?! (car tail)) (apply @let/ env tail))))
(define (@begin env . body) (foldl (lambda (form _) (eval env form)) #t body))
(define (@and env . body)
  (foldl (lambda (form v) (and v (eval env form))) #t body))
(define (@or env . body)
  (foldl (lambda (form v) (or v (eval env form))) #f body))
(define (@when env c . body)   (if (eval env c) (@body* env body) #t))
(define (@unless env c . body) (if (eval env c) #t (@body* env body)))
(define (@cond env . clauses)
  (if (null? clauses) #t
    (let ((? (eval env (caar clauses))))
      (if ? (if (null? (cdar clauses)) ? (@body* env (cdar clauses)))
        (apply @cond env (cdr clauses))))))

(define (defstate-actions-add-expr st form)
  (defstate-actions-add st (lambda (env) (eval env form))))
(define (defstate-run st)
  (define env (defstate-env st))
  (foldl (lambda (act _) (act env)) #t (reverse (defstate-actions st))))
(define (@begin/define st . forms)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($def (and n (env-ref-prop (defstate-env st) n ctx:def #f))))
             (if $def ($apply $def (cons st (cdr form)))
               (defstate-actions-add-expr st form)))) st forms))
(define (@def st param arg)
  (define names (param-names param))
  (defstate-actions-add
    (defstate-env-set (defstate-names-add st names)
                      (env-extend*/var (alist-remove* (defstate-env st) names)
                                       (map (lambda (n) (cons n #t)) names)))
    (lambda (env) (@set! env param arg))))
(define (@define st param . body)
  (if (pair? param)
    (@define st (car param)
             (plift (lambda (env) (apply @lambda env (cdr param) body))))
    (@def st param (car body))))
(define (@body* env body)
  (defstate-run (apply @begin/define (defstate:empty env) body)))

;; Base environment construction
(define (ref-proc p) (cons ctx:var (lambda _ (plift p))))

(define env:primitive
  (s->ns
    (append
      (map (lambda (b) (cons (car b) (list (ref-proc (cdr b))
                                           (cons ctx:op #t))))
           (list (cons 'if     @if)
                 (cons 'reset  @reset)
                 (cons 'shift  @shift)
                 (cons 'lambda @lambda)
                 ;; TODO: this won't work due to Racket's list-based apply.
                 ;(cons '$ (lambda (env rator . rands)
                            ;((eval env rator) (cons env rands))))
                 ))
      ;; TODO: in nScheme self-interpreter, this version won't be necessary.
      (map (lambda (b) (cons (car b) (list (cons ctx:var (lambda _ (cdr b)))
                                           (cons ctx:op #t))))
           (list (cons '$ (lambda (args) ((eval (car args) (cadr args))
                                          (cons (car args) (cddr args)))))))
      (map (lambda (b) (cons (car b) (list (ref-proc (cdr b)))))
           (list (cons 'eval        eval)
                 (cons 'apply       $apply)
                 (cons 'procedure?  procedure?)
                 (cons 'mvector?    mvector?)
                 (cons 'vector?     vector?)
                 (cons 'pair?       pair?)
                 (cons 'null?       null?)
                 (cons 'boolean?    boolean?)
                 (cons 'string?     string?)
                 (cons 'number?     number?)
                 (cons 'integer?    integer?)
                 (cons 'fixnum?     fixnum?)
                 (cons 'flonum?     flonum?)
                 (cons 'boolean=?   boolean=?)
                 (cons 'number=?    eqv?)
                 (cons 'string=?    string=?)
                 (cons 'mvector=?   mvector=?)
                 (cons 'procedure=? procedure=?)
                 (cons 'string->vector string->vector)
                 (cons 'vector->string vector->string)
                 (cons 'cons cons)
                 (cons 'car  car)
                 (cons 'cdr  cdr)
                 (cons 'vector-ref    vector-ref)
                 (cons 'vector-length vector-length)
                 (cons 'make-mvector    make-mvector)
                 (cons 'mvector->vector mvector->vector)
                 (cons 'mvector-set!    mvector-set!)
                 (cons 'mvector-ref     mvector-ref)
                 (cons 'mvector-length  mvector-length)
                 (cons 'string<?        string<?)
                 (cons 'string>?        string>?)
                 (cons '=  =)
                 (cons '<= <=)
                 (cons '<  <)
                 (cons '>= >=)
                 (cons '>  >)
                 (cons '+  +)
                 (cons '*  *)
                 (cons '-  -)
                 (cons '/  /)
                 ;; TODO: these and others?
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
                 ))
      ;; These don't have to be primitive, but are provided for convenience.
      (list (cons 'define (list (cons 'define (plift @define))))
            (cons 'def    (list (cons 'define (plift @def))))
            (cons 'begin  (list (ref-proc @begin) (cons ctx:op #t)
                                (cons 'define (plift @begin/define)))))
      (map (lambda (b) (cons (car b) (list (ref-proc (cdr b))
                                           (cons ctx:op #t))))
           (list (cons 'quote  @quote)
                 (cons 'set!   @set!)
                 (cons 'let    @let)
                 (cons 'let*   @let*)
                 (cons 'letrec @letrec)
                 (cons 'and    @and)
                 (cons 'or     @or)
                 (cons 'when   @when)
                 (cons 'unless @unless)
                 (cons 'cond   @cond))))))

(define derived-ops
  '((error (lambda args ('error args)))
    (not (lambda (b) (if b #f #t)))
    (caar (lambda (v) (car (car v))))
    (cadr  (lambda (xs) (car (cdr xs))))
    (cadar (lambda (v) (cadr (car v))))
    (caddr (lambda (xs) (cadr (cdr xs))))
    (list-tail (lambda (xs i) (if (= 0 i) xs (list-tail (cdr xs) (- i 1)))))
    (list-ref  (lambda (xs i) (car (list-tail xs i))))
    (list->vector (lambda (xs)
                    (define result (make-mvector (length xs) #t))
                    (foldl (lambda (x i) (mvector-set! result i x) (+ i 1))
                           0 xs)
                    (mvector->vector result)))
    (vector->list (lambda (v)
                    (let loop ((i (- (vector-length v) 1)) (xs '()))
                      (if (< i 0) xs
                        (loop (- i 1) (cons (vector-ref v i) xs))))))
    (equal? (lambda (a b)
              (cond ((pair? a)   (and (pair? b) (equal? (car a) (car b))
                                      (equal? (cdr a) (cdr b))))
                    ((vector? a) (and (vector? b) (equal? (vector->list a)
                                                          (vector->list b))))
                    ((boolean? a)   (and (boolean? b)   (boolean=? a b)))
                    ((string?  a)   (and (string?  b)   (string=?  a b)))
                    ((number?  a)   (and (number?  b)   (number=?  a b)))
                    ((mvector? a)   (and (mvector? b)   (mvector=? a b)))
                    ((procedure? a) (and (procedure? b) (procedure=? a b)))
                    ((null? a)      (null? b)))))
    (vector (lambda xs (list->vector xs)))
    (list?  (lambda (v) (or (and (pair? v) (list? (cdr v))) (null? v))))
    (list   (lambda xs xs))
    (list*  (lambda (x . xs) (if (null? xs) x (cons x (apply list* xs)))))
    (foldl  (lambda (f acc xs) (if (null? xs) acc
                                 (foldl f (f (car xs) acc) (cdr xs)))))
    (foldr  (lambda (f acc xs) (if (null? xs) acc
                                 (f (car xs) (foldr f acc (cdr xs))))))
    (map (lambda (f xs . xss)
           (define (map1 f xs) (if (null? xs) '()
                                 (cons (f (car xs)) (map1 f (cdr xs)))))
           (cond ((null? xs) '())
                 (#t (cons (apply f (car xs) (map1 car xss))
                           (apply map f (cdr xs) (map1 cdr xss)))))))
    (for-each (lambda args (apply map args) #t))
    (andmap (lambda (f xs . xss)
              (let loop ((last #t) (xs xs) (xss xss))
                (and last (if (null? xs) last
                            (loop (apply f (car xs) (map car xss))
                                  (cdr xs) (map cdr xss)))))))
    (ormap (lambda (f xs . xss)
             (cond ((null? xs) #f)
                   ((apply f (car xs) (map car xss)))
                   (#t (apply ormap f (cdr xs) (map cdr xss))))))
    (filter (lambda (p? xs)
              (cond ((null? xs) '())
                    ((p? (car xs)) (cons (car xs) (filter p? (cdr xs))))
                    (#t (filter p? (cdr xs))))))
    (filter-not (lambda (p? xs) (filter (lambda (x) (not (p? x))) xs)))
    (remf (lambda (p? xs)
            (cond ((null? xs)    '())
                  ((p? (car xs)) (cdr xs))
                  (#t (cons (car xs) (remf p? (cdr xs)))))))
    (remove (lambda (v xs) (remf (lambda (x) (equal? x v)) xs)))
    (length (lambda (xs) (foldl (lambda (_ l) (+ 1 l)) 0 xs)))
    (append (lambda xss (foldr (lambda (xs yss) (foldr cons yss xs)) '() xss)))
    (reverse-append (lambda (xs ys) (foldl cons ys xs)))
    (reverse (lambda (xs) (reverse-append xs '())))
    (range (lambda (n)
             (let loop ((i 0)) (if (= i n) '() (cons i (loop (+ i 1)))))))
    (take (lambda (xs n) (if (= 0 n) '()
                           (cons (car xs) (take (cdr xs) (- n 1))))))
    (drop (lambda (xs n) (if (= 0 n) xs (drop (cdr xs) (- n 1)))))
    (memf (lambda (? xs) (cond ((null? xs) #f)
                               ((? (car xs)) xs)
                               (#t (memf ? (cdr xs))))))
    (member (lambda (v xs) (memf (lambda (x) (equal? x v)) xs)))
    (assoc (lambda (k xs) (cond ((null? xs) #f)
                                ((equal? k (caar xs)) (car xs))
                                (#t (assoc k (cdr xs))))))
    (alist-ref (lambda (rs key default) (let ((rib (assoc key rs)))
                                          (if rib (cdr rib) default))))
    (alist-remove* (lambda (rs keys)
                     (filter (lambda (rib) (not (member (car rib) keys))) rs)))
    (string-append (lambda ss
                     (define css (map vector->list (map string->vector ss)))
                     (vector->string (list->vector (apply append css)))))
    ))

(define env:base
  (eval env:primitive
        (s->ns `(let ((apply (lambda (f arg . args)
                               (define (cons* x xs)
                                 (if (null? xs) x
                                   (cons x (cons* (car xs) (cdr xs)))))
                               (apply f (cons* arg args)))))
                  (letrec ,derived-ops ($ (lambda (env) env)))))))

;; Tests:
(define tests-total 0)
(define test-failures '())

(define (test-report)
  (define tests-failed (length test-failures))
  (define tests-passed (- tests-total tests-failed))
  (printf "********************************\nTests passed: ~a out of ~a\n"
          tests-passed tests-total)
  (unless (= tests-passed tests-total)
    (printf "Tests failed: ~a out of ~a\n" tests-failed tests-total)
    (printf "~s\n" test-failures)))

(define (test name actual expected)
  (printf "Testing ~a: " name)
  (set! tests-total (+ tests-total 1))
  (cond ((equal? expected actual) (printf "Succeeded.\n"))
        (else (printf "Failed.\nExpected: ~s\nActual: ~s\n****************\n"
                      expected actual)
              (set! test-failures (cons name test-failures)))))

(define (ev code) (eval env:base code))

(test '$-1  ;; $ applies a procedure with current environment and raw syntax.
 (ev '($ (lambda (env . tree) tree) 4 5))
 '(4 5))
(test '$-2  ;; We can even apply such procedures with improper argument lists.
 (ev '($ (lambda (env . tree) tree) 4 . 5))
 '(4 . 5))
(test '$-3
  (ev '($ (lambda (env) (((cdr (assoc 'ref (cdr (assoc 'vector env)))))
                         1 2 3))))
  '#(1 2 3))

(test 'lambda-1  ;; Formal parameter lists are generalized to arbitrary trees.
  (ev '((lambda (() a (b)) (cons a b))
        '() 1 '(2)))
  '(1 . 2))
(test 'lambda-2  ;; Formal parameter lists are generalized to arbitrary trees.
  (ev '((lambda (() a #(b c)) (cons a (cons b (cons c '()))))
        '() 1 '#(2 3)))
  '(1 2 3))
(test 'lambda-3
  (ev '((lambda (#f x #f) x) 1 2 3))
  2)
(test 'define-1
  (ev '((lambda (() a #(b c))
          (define x (lambda () (+ (+ c y) z)))
          (define y b)
          (define z a)
          (x))
        '() 1 '#(2 3)))
  6)
(test 'define-2
  (ev '((lambda (() a #(b c))
          (begin (define x (lambda () (+ (+ c y) z)))
                 (define y b))
          (define z a)
          (x))
        '() 1 '#(2 3)))
  6)
(test 'define-3
  (ev '((lambda (() a #(b c))
          (define ((f w) x y) (list w x y))
          ((f a) b c))
        '() 1 '#(2 3)))
  '(1 2 3))

(test 'apply-lambda-1
  (ev '((apply lambda (cons '()         ;; empty env
                            '(_ 5)))))  ;; quote is unbound
  5)
(test 'apply-lambda-2
  (ev `((apply lambda (cons ,(plift (lambda (env) env))  ;; spliced evaluator
                            '(_ '5)))))                  ;; quote is bound
  5)
(test 'apply-lambda-3
  (ev '((apply lambda (cons ($ (lambda (env) env))  ;; evaluator via $
                            '(_ '5)))))             ;; quote is bound
  5)
(test 'apply-lambda-4
  (ev '((((lambda (x) x) lambda)  ;; another applicative lambda example
         ($ (lambda (env) env)) '_ ''5)))
  5)

(test 'literals
  (map ev (list '(quote ()) '#t '4))
  '(() #t 4))

(test 'pair-1
  (ev '(pair? '(x x)))
  #t)
(test 'pair-2
  (ev '(pair? #t))
  #f)
(test 'pair-3
  (ev '(pair? (lambda x x)))
  #f)

(test 'procedure-1
  (ev '(procedure? '(x x)))
  #f)
(test 'procedure-2
  (ev '(procedure? '#t))
  #f)
(test 'procedure-3
  (ev '(procedure? (lambda x x)))
  #t)

(test 'lambda-shadowing-1
  (ev '((lambda lambda lambda) 'ok))
  '(ok))

(test 'lambda-app-1
  (ev '((lambda (x y) x) 5 6))
  5)
(test 'lambda-app-2
  (ev '((lambda (x y) y) 5 6))
  6)
(test 'lambda-app-3
  (ev '((lambda (x y) (cons y x)) 5 6))
  '(6 . 5))
(test 'lambda-app-4
  (ev '((lambda (x) (cons (cdr x) (car x))) (cons #t #f)))
  '(#f . #t))

(test 'let-1
  (ev '(let ((x 8)) x))
  8)
(test 'let-2
  (ev '(let ((x 9)) (let ((x 20)) x)))
  20)
(test 'let-3
  (ev '(let ((x 9)) (let ((y 20)) x)))
  9)
(test 'let-4
  (ev '(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
  10)
(test 'let-5
  (ev '(let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
  (let ((x 10) (y 4)) (let ((x 11) (y x)) y)))
(test 'let-6
  (ev '(let ((op (lambda (x) (car x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
         (if (op datum) ta fa)))
  'yes)
(test 'let-7
  (ev '(let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
         (if (op datum) ta fa)))
  'no)
(test 'let-8
  (ev '(let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
         (if (op datum) ta fa)))
  (let ((op (lambda (x) (cdr x))) (datum '(#t . #f)) (ta 'yes) (fa 'no))
    (if (op datum) ta fa)))
(test 'let-9
  (ev '(let loop ((xs '(a b c)) (acc '()))
         (if (null? xs) acc
           (loop (cdr xs) (cons (car xs) acc)))))
  '(c b a))

(test 'internal-defs-1
  (ev '(let ((x 1) (y 7) (z 33))
         (define u 88)
         (define z y)
         6
         (begin (define a 5) (define w 4))
         z))
  7)
(test 'internal-defs-2
  (ev '(let ((x 1) (y 7) (z 33))
         (define y 88)
         (define z y)
         6
         (begin (define a 5) (define w 4))
         z))
  88)
(test 'internal-defs-3
  (ev '(let ((x 1) (y 7) (z 33))
         (define y 88)
         (define z y)
         6
         (begin (define a 5) (define w 4))
         a))
  5)
(test 'internal-defs-4
  (ev '(let ((x 1) (y 7) (z 33))
         (define y 88)
         (define z (lambda (x) x))
         6
         (begin (define a 5) (define w (z y)))
         w))
  88)

(test 'set-1
  (ev '(let ((x 0)) (set! x 2) x))
  2)
(test 'set-2
  (ev '(let ((x 0)) (define y x) (set! x 2) y))
  0)

(test 'letrec-1
  (ev '(letrec ((w (lambda () (y)))
                (x 32)
                (y (lambda () x)))
         (w)))
  32)
(test 'letrec-2
  (ev '(letrec ((w (lambda () y))
                (x 33)
                (y x))
         (w)))
  33)

(test 'begin-1
  (ev '(begin 1))
  1)
(test 'begin-2
  (ev '(begin 1 2))
  2)
(test 'begin-3
  (ev '(let ((x 1))
         (let ((y (begin (set! x 6) x)))
           y)))
  6)

(test 'if-1
  (ev '(if #t 'yes 'no))
  'yes)
(test 'if-2
  (ev '(if #f 'yes 'no))
  'no)
(test 'if-3
  (ev '(if 0 'yes 'no))
  'yes)
(test 'if-4
  (ev '(if (car '(#t . #f)) 'yes 'no))
  'yes)
(test 'if-5
  (ev '(if (cdr '(#t . #f)) 'yes 'no))
  'no)

(test 'and-1
  (ev '(and))
  #t)
(test 'and-2
  (ev '(and 1))
  1)
(test 'and-3
  (ev '(and #f 2))
  #f)
(test 'and-4
  (ev '(and 2 3))
  3)
(test 'and-5
  (ev '(and 2 3 4))
  4)
(test 'and-6
  (ev '(and 2 #f 4))
  #f)

(test 'or-1
  (ev '(or))
  #f)
(test 'or-2
  (ev '(or 1))
  1)
(test 'or-3
  (ev '(or #f 2))
  2)
(test 'or-4
  (ev '(or 2 3))
  2)
(test 'or-5
  (ev '(or #f #f 4))
  4)
(test 'or-6
  (ev '(or 2 #f 4))
  2)

(test 'when-1
  (ev '(when 1 2))
  2)
(test 'when-2
  (ev '(when #f 3))
  #t)

(test 'unless-1
  (ev '(unless 1 2))
  #t)
(test 'unless-2
  (ev '(unless #f 3))
  3)

(test 'cond-1
  (ev '(cond (1 2)))
  2)
(test 'cond-2
  (ev '(cond (#f 3)
             (4 5)))
  5)
(test 'cond-3
  (ev '(cond (#f 3)
             (8)
             (4 5)))
  8)

(test 'misc-1
  (ev '((lambda (w #f x #f y . z)
          (if (x y z '(a ... z))
            'true
            'false))
        1 2 (lambda x x) 4 5 6 7))
  'true)
(test 'misc-2
  (ev '((lambda (w #f x #f y . z)
          (if (x y z '(a ... z))
            'true
            'false))
        1 2 (lambda x #f) 4 5 6 7))
  'false)
(test 'misc-3
  (ev '((lambda (w #f x #f y . z)
          (if 'true
            (x y z '(a ... z))
            'false))
        1 2 (lambda x x) 4 5 6 7))
  '(5 (6 7) (a ... z)))

(test 'let*-1
  (ev '(let* ((a 1) (b (cons 2 a))) b))
  '(2 . 1))
(test 'let*-2
  (ev '(let* ((a 1) (b (cons 2 a))) b))
  (let* ((a 1) (b (cons 2 a))) b))

(test 'shift/reset-1
  (ev '(* 2 (reset (+ 1 (shift k (k 5))))))
  12)
(test 'shift/reset-2
  (ev '(reset (* 2 (shift k (k (k 4))))))
  16)

(test 'fix-0
  (ev '(let ((list (lambda xs xs))
             (fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda (a b) ((x x) a b))))))))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           (list (append '() '())
                 (append '(foo) '(bar))
                 (append '(1 2) '(3 4))))))
  '(() (foo bar) (1 2 3 4)))

(test 'fix-1
  (ev '(let ((list (lambda xs xs))
             (fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           (list (append '() '())
                 (append '(foo) '(bar))
                 (append '(1 2) '(3 4))))))
  '(() (foo bar) (1 2 3 4)))

(test 'fix-2
  (ev '(let ((list (lambda xs xs))
             (fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           (list (append '() '())
                 (append '(foo) '(bar))
                 (append '(1 2) '(3 4))))))
  (let ((list (lambda xs xs))
        (fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda a (apply (x x) a))))))))
    (let ((append
            (fix (lambda (append)
                   (lambda (xs ys)
                     (if (null? xs)
                       ys
                       (cons (car xs) (append (cdr xs) ys))))))))
      (list (append '() '())
            (append '(foo) '(bar))
            (append '(1 2) '(3 4))))))

(test 'fix*-1
  (ev '(let ((list (lambda xs xs))
             (fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((map (fix (lambda (map)
                           (lambda (f xs)
                             (if (null? xs)
                               '()
                               (cons (f (car xs)) (map f (cdr xs)))))))))
           (let ((fix*
                   (fix (lambda (fix*)
                          (lambda fs
                            (map (lambda (fi)
                                   (lambda a
                                     (apply (apply fi (apply fix* fs)) a)))
                                 fs))))))
             (let ((even&odd
                     (fix* (lambda (even? odd?)
                             (lambda (n) (if (null? n)
                                           #t
                                           (odd? (cdr n)))))
                           (lambda (even? odd?)
                             (lambda (n)
                               (if (null? n)
                                 #f
                                 (even? (cdr n))))))))
               (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
                 (list (even? '())    (odd? '())
                       (even? '(s))   (odd? '(s))
                       (even? '(s s)) (odd? '(s s)))))))))
  '(#t #f #f #t #t #f))

(test 'fix*-2
  (ev '(let ((list (lambda xs xs))
             (fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((map (fix (lambda (map)
                           (lambda (f xs)
                             (if (null? xs)
                               '()
                               (cons (f (car xs)) (map f (cdr xs)))))))))
           (let ((fix*
                   (fix (lambda (fix*)
                          (lambda fs
                            (map (lambda (fi)
                                   (lambda a
                                     (apply (apply fi (apply fix* fs)) a)))
                                 fs))))))
             (let ((even&odd
                     (fix* (lambda (even? odd?)
                             (lambda (n) (if (null? n)
                                           #t
                                           (odd? (cdr n)))))
                           (lambda (even? odd?)
                             (lambda (n)
                               (if (null? n)
                                 #f
                                 (even? (cdr n))))))))
               (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
                 (list (even? '())    (odd? '())
                       (even? '(s))   (odd? '(s))
                       (even? '(s s)) (odd? '(s s)))))))))
  (let ((list (lambda xs xs))
        (fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda a (apply (x x) a))))))))
    (let ((map (fix (lambda (map)
                      (lambda (f xs)
                        (if (null? xs)
                          '()
                          (cons (f (car xs)) (map f (cdr xs)))))))))
      (let ((fix*
              (fix (lambda (fix*)
                     (lambda fs
                       (map (lambda (fi)
                              (lambda a
                                (apply (apply fi (apply fix* fs)) a)))
                            fs))))))
        (let ((even&odd
                (fix* (lambda (even? odd?)
                        (lambda (n) (if (null? n)
                                      #t
                                      (odd? (cdr n)))))
                      (lambda (even? odd?)
                        (lambda (n)
                          (if (null? n)
                            #f
                            (even? (cdr n))))))))
          (let ((even? (car even&odd)) (odd? (car (cdr even&odd))))
            (list (even? '())    (odd? '())
                  (even? '(s))   (odd? '(s))
                  (even? '(s s)) (odd? '(s s)))))))))

(test 'vector-1
  (map ev '((vector)
            (vector 3 1)))
  '(#() #(3 1)))
(test 'vector-2
  (map ev '((vector-length (vector))
            (vector-length (vector 3 1))))
  '(0 2))
(test 'vector-3
  (map ev '((vector-ref (vector 5) 0)
            (vector-ref (vector 3 1 2) 0)
            (vector-ref (vector 3 1 2) 1)))
  '(5 3 1))
(test 'vector-4
  (map ev '((vector? (vector))
            (vector? (vector 3 1))
            (vector? '(x x))
            (vector? (lambda x x))))
  '(#t #t #f #f))
(test 'vector-5
  (map ev '((vector-ref '#(1 2 3) 0)
            (vector-ref '#(4 5 6) 2)
            (vector-ref '#(7 8 9) 1)
            (vector-ref (car (cdr (list 6 (vector 7 (cons 8 9) 0) 1))) 1)
            (vector-ref (car (cdr (list 6 (vector
                                            7 (cons 8 9) 0 (car '(10 . #f)))
                                        1))) 3)))
  '(1 6 8 (8 . 9) 10))

(test 'list-1
  (map ev '((list)
            (list 6 7)))
  '(() (6 7)))
(test 'list-2
  (map ev '((list? (list))
            (list? (list 6 7))
            (list? '(6 . 7))))
  '(#t #t #f))

(test 'pair-4
  (ev '(pair? (vector 3 1 2)))
  #f)
(test 'procedure-5
  (ev '(procedure? (vector 3 1 2)))
  #f)

(test 'equal-1
  (ev '(map equal?
            '(9 9 () #t #f one two)
            '(9 8 () #t #t one one)))
  '(#t #f #t #t #f #t #f))
(test 'equal-2
  (ev '(map equal?
            '((5 6) (4 6) (5 7) #(1 2 3) #(1 8 3))
            '((5 6) (5 6) (5 6) #(1 2 3) #(1 2 3))))
  '(#t #f #f #t #f))
(test 'equal-3
  (ev '(let ((id (lambda (x) x)))
         (map equal? (list id id) (list id (lambda (x) x)))))
  '(#t #f))


(test-report)