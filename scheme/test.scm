(define-syntax test
  (syntax-rules ()
    ((_ name expr expected-expr)
     (begin
       (printf "Testing ~s: " name)
       (let* ((expected expected-expr) (actual expr))
         (if (equal? expected actual)
           (printf "Succeeded.\n")
           (printf "\nFailed: ~a\nExpected: ~a\nActual: ~a\n"
                   'expr expected actual)))))))

(define (ev expr) (evaluate expr env-initial))

(test 'literals
  (map ev '('() #t 4))
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
(test 'pair-4
  (ev '(pair? apply))
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
(test 'procedure-4
  (ev '(procedure? apply))
  #t)

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
(test 'lambda-app-5
  (ev '(apply (lambda x x) '(1 2 3)))
  '(1 2 3))
(test 'lambda-app-6
  (ev '(apply (lambda (a b c) b) '(1 2 3)))
  2)
(test 'lambda-app-7
  (ev '(apply (lambda x x) '(1 2 3)))
  (apply (lambda x x) '(1 2 3)))
(test 'lambda-app-8
  (ev '(apply (lambda (a b c) b) '(1 2 3)))
  (apply (lambda (a b c) b) '(1 2 3)))


(test 'if-1
  (ev '(if (car '(#t . #f)) 'yes 'no))
  'yes)
(test 'if-1
  (ev '(if (cdr '(#t . #f)) 'yes 'no))
  'no)

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

(test 'qq-1
  (ev '`one)
  'one)
(test 'qq-2
  (ev '`((a 1) (b 2)))
  '((a 1) (b 2)))
(test 'qq-3
  (ev '`((a 1) ,(cons 'b `(3))))
  '((a 1) (b 3)))
(test 'qq-4
 (ev '`(1 `,(cons 2 3) ,(cons 4 5) `(,(cons 6 ,(cons 7 8))) 100))
 `(1 `,(cons 2 3) ,(cons 4 5) `(,(cons 6 ,(cons 7 8))) 100))
