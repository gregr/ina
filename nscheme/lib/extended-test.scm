((require test module:base-primitive module:base language:extended
          premodule module:premodule module-apply namespace-link*))

(define ns (namespace-link* '() (list module:base-primitive module:base)))
(define name=>lang (list (cons 'extended language:extended)))
(define (ev form)
  (define pm (premodule '() #f '() #f '(extended) form))
  (module-apply (module:premodule name=>lang pm) ns))

(test 'cond-3
  (ev '(cond (#f 3)
             (8)
             (4 5)))
  8)
(test 'cond-4
  (ev '(cond (#f 3)
             ((car '(#f 4)) 5)
             (else 6)))
  6)
(test 'cond-5
  (ev '(cond (#f 3)
             ((car '(#f 4)) 5)
             ('the => (lambda (v) (cons v 'answer)))
             (else 6)))
  '(the . answer))

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
  (list 1 '`,(cons 2 3) (cons 4 5)
        (list 'quasiquote (list (list 'unquote (list 'cons 6 (cons 7 8)))))
        100))
(test 'qq-5
  (ev '`(1 ,@(cons 2 (cons 3 '())) 4))
  '(1 2 3 4))
(test 'qq-6
  (ev '`#(1 ,(cons 2 (cons 3 '())) 4))
  '#(1 (2 3) 4))
(test 'qq-7
  (ev '`#(1 ,@(cons 2 (cons 3 '())) 4))
  '#(1 2 3 4))

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
