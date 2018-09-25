(provide test!)

(require eval/ast nscheme:expand)

(define (ev form) (eval/ast (nscheme:expand form)))

(define (test! test)
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
           (define (z x) x)
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
               ((car '(#f 4)) 5)
               (else 6)))
    6)
  (test 'cond-4
    (ev '(cond (#f 3)
               ((car '(#f 4)) 5)
               ('the => (lambda (v) (cons v 'answer)))
               (else 6)))
    '(the . answer))
  (test 'cond-5
    (ev '(cond (8)))
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
  (test 'misc-2
    (ev '((lambda (w #f x #f y . z)
            (if 'true
              (x y z '(a ... z))
              'false))
          1 2 (lambda x x) 4 5 6 7))
    '(5 (6 7) (a ... z)))

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
  (test 'qq-5
    (ev '`(1 ,@(cons 2 (cons 3 '())) 4))
    '(1 2 3 4))
  (test 'qq-6
    (ev '`#(1 ,(cons 2 (cons 3 '())) 4))
    '#(1 (2 3) 4))
  (test 'qq-7
    (ev '`#(1 ,@(cons 2 (cons 3 '())) 4))
    '#(1 2 3 4))

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
              (vector-ref `#(7 8 9) 1)
              (vector-ref (car (cdr `(6 #(7 ,(cons 8 9) 0) 1))) 1)
              (vector-ref (car (cdr `(6 #(7 ,(cons 8 9) 0 ,(car '(10 . #f))) 1))) 3)))
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

  (test 'case-1
    (ev '(case 3 (else 'ok)))
    'ok)
  (test 'case-2
    (ev '(case 3
           ((4) 'four)
           (((3 4)) 'multiple)
           ((3) 'three)
           ((foo bar) 'foobar)
           (else 'fail)))
    'three)
  (test 'case-3
    (ev '(case 'foo
           ((4) 'four)
           (((3 4)) 'multiple)
           ((3) 'three)
           ((foo bar) 'foobar)
           (else 'fail)))
    'foobar)
  (test 'case-4
    (ev '(case 'bar
           ((4) 'four)
           (((3 4)) 'multiple)
           ((3) 'three)
           ((foo bar) 'foobar)
           (else 'fail)))
    'foobar)
  (test 'case-5
    (ev '(case 'baz
           ((4) 'four)
           (((3 4)) 'multiple)
           ((3) 'three)
           ((foo bar) 'foobar)
           (else 'fail)))
    'fail)
  (test 'case-6
    (ev '(case '(3 4)
           ((4) 'four)
           (((3 4)) 'multiple)
           ((3) 'three)
           ((foo bar) 'foobar)
           (else 'fail)))
    'multiple)

  (test 'match-1
    (ev '(match 3
           (2              'two)
           (3              'three)
           (`(,x ,y ,z)    `(proper: ,x ,y ,z))
           ((list x w)     `(list: ,x ,w))
           (`(,x ,y ,@z)   `(splice: ,x ,y ,z))
           (`(,x ,y . ,z)  `(improper: ,x ,y ,z))
           ((list* x y)    `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z)  `(vector-splice: ,x ,y ,z))
           ('foo           'bar)
           (_              'fail)))
    'three)
  (test 'match-2
    (ev '(match 'foo
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    'bar)
  (test 'match-3
    (ev '(match 5
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    'fail)
  (test 'match-4
    (ev '(match '(a b c)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(proper: a b c))
  (test 'match-5
    (ev '(match '(a b c d)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(splice: a b (c d)))
  (test 'match-6
    (ev '(match '(a b c . d)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(improper: a b (c . d)))
  (test 'match-7
    (ev '(match '(a . b)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(cons: a b))
  (test 'match-8
    (ev '(match '(a b)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(list: a b))
  (test 'match-9
    (ev '(match '#(a b c d)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(vector-splice: a b (c d)))
  (test 'match-10
    (ev '(match '#(a b c)
           (2 'two)
           (3 'three)
           (`(,x ,y ,z) `(proper: ,x ,y ,z))
           ((list x y) `(list: ,x ,y))
           (`(,x ,y ,@z) `(splice: ,x ,y ,z))
           (`(,x ,y . ,z) `(improper: ,x ,y ,z))
           ((list* x y) `(cons: ,x ,y))
           ((vector x y z) `(vector: ,x ,y ,z))
           (`#(,x ,y ,@z) `(vector-splice: ,x ,y ,z))
           ('foo 'bar)
           (_ 'fail)))
    '(vector: a b c))
  (test 'match-11
    (ev '(match '((1) #t (three (4 five) six) 7) loop
           ('() '())
           (`(,a . ,d) (cons (loop a) (loop d)))
           (n (guard (number? n)) 'n)
           (s (guard (string? s)) 's)
           (_ '?)))
    '((n) ? (s (n s) s) n))
  )
