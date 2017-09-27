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

(test 'vector-1
  (map vector-reify (map ev '((vector)
                              (vector 5)
                              (vector 3 1 2))))
  '(#() #(5) #(3 1 2)))
(test 'vector-2
  (map ev '((vector-length (vector))
            (vector-length (vector 5))
            (vector-length (vector 3 1 2))))
  '(0 1 3))
(test 'vector-3
  (map ev '((vector-ref (vector 5) 0)
            (vector-ref (vector 3 1 2) 0)
            (vector-ref (vector 3 1 2) 1)
            (vector-ref (vector 3 1 2) 2)))
  '(5 3 1 2))
(test 'vector-4
  (map ev '((vector? (vector))
            (vector? (vector 5))
            (vector? (vector 3 1 2))
            (vector? #t)
            (vector? '(x x))
            (vector? (lambda x x))
            (vector? apply)))
  '(#t #t #t #f #f #f #f))
(test 'vector-5
  (map ev '((vector-ref #(1 2 3) 0)
            (vector-ref '#(4 5 6) 2)
            (vector-ref `#(7 8 9) 1)
            (vector-ref (car (cdr `(6 #(7 ,(cons 8 9) 0) 1))) 1)))
  '(1 6 8 (8 . 9)))

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
(test 'pair-5
  (ev '(pair? (vector 3 1 2)))
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
(test 'procedure-5
  (ev '(procedure? (vector 3 1 2)))
  #f)

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
(test 'if-2
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

(test 'let*-1
  (ev '(let* ((a 1) (b `(2 ,a))) b))
  '(2 1))
(test 'let*-2
  (ev '(let* ((a 1) (b `(2 ,a))) b))
  (let* ((a 1) (b `(2 ,a))) b))

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

(test 'fix-1
  (ev '(let ((fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           `(,(append '() '())
             ,(append '(foo) '(bar))
             ,(append '(1 2) '(3 4))))))
  '(() (foo bar) (1 2 3 4)))

(test 'fix-2
  (ev '(let ((fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a))))))))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           `(,(append '() '())
             ,(append '(foo) '(bar))
             ,(append '(1 2) '(3 4))))))
  (let ((fix (lambda (f)
               ((lambda (d) (d d))
                (lambda (x) (f (lambda a (apply (x x) a))))))))
    (let ((append
            (fix (lambda (append)
                   (lambda (xs ys)
                     (if (null? xs)
                       ys
                       (cons (car xs) (append (cdr xs) ys))))))))
      `(,(append '() '())
         ,(append '(foo) '(bar))
         ,(append '(1 2) '(3 4))))))

(test 'fix*-1
  (ev '(let ((fix (lambda (f)
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
                 `(,(even? '()) ,(odd? '())
                                ,(even? '(s))   ,(odd? '(s))
                                ,(even? '(s s)) ,(odd? '(s s)))))))))
  '(#t #f #f #t #t #f))

(test 'fix*-2
  (ev '(let ((fix (lambda (f)
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
                 `(,(even? '()) ,(odd? '())
                                ,(even? '(s))   ,(odd? '(s))
                                ,(even? '(s s)) ,(odd? '(s s)))))))))
  (let ((fix (lambda (f)
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
            `(,(even? '()) ,(odd? '())
                           ,(even? '(s))   ,(odd? '(s))
                           ,(even? '(s s)) ,(odd? '(s s)))))))))
