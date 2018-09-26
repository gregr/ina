(provide test!)

(require eval/ast expand expander nscheme:expand env:base)

(define (make-syntax=? context env-a env-b)
  (define (=? a b)
    (cond ((and (pair? a) (pair? b)) (and (=? (car a) (car b))
                                          (=? (cdr a) (cdr b))))
          ((and (name? a) (name? b))
           (equal? (binding-uid (env-ref env-a (list context) a))
                   (binding-uid (env-ref env-b (list context) b))))
          (#t (equal? a b))))
  =?)

(define ($pair? x)          (ast:primitive-op 'pair?   (list x)))
(define ($car x)            (ast:primitive-op 'car     (list x)))
(define ($cdr x)            (ast:primitive-op 'cdr     (list x)))
(define ($vector? x)        (ast:primitive-op 'vector? (list x)))
(define ($thunk body)       (ast:lambda #f '() body))

;; TODO: define these in userspace after bootstrapping $-based extension.
,(
(define (parser-descs->b* qenv descs)
  (cons 'list
        (map (lambda (name&clause*)
               (list 'cons (list 'quote (car name&clause*))
                     (list 'lambda '(env form)
                           '(define (ex form) (expand env form))
                           '(define (@ i) (list-ref form i))
                           '(define (@. i) (list-tail form i))
                           (cons 'cond (append
                                         (cdr name&clause*)
                                         '((#t (error '"invalid syntax:"
                                                      form))))))))
             descs)))

;; Parsers with dependencies on base definitions
(define parsers:future
  '((cond
      ((length>=? 2 form)
       (define =? (make-syntax=? ctx:var env:primitive-syntax env))
       (unless (length>=? 1 (@. 1)) (error '"invalid cond:" form))
       (let loop ((c* (@. 1)))
         (cond
           ((null? c*) ($error (ast:quote '"no matching cond clause:")
                               (ast:quote form)))
           ((not (length>=? 1 (car c*))) (error '"invalid cond:" form))
           ((and (=? 'else (caar c*)) (length=? 1 c*))
            (expand:body* env (cdar c*)))
           (#t (let* (($* (loop (cdr c*))) (c0 (caar c*)) (c. (cdar c*)))
                 (cond ((=? 'else c0) (error '"invalid else in cond:" form))
                       ((null? c.)    (expand:or2 env c0 $*))
                       ((and (=? '=> (car c.)) (length=? 1 (cdr c.)))
                        (define ($t->body $t)
                          (ast:if $t ($apply* (ex (cadr c.)) (list $t)) $*))
                        (expand:let/temp 'temp env c0 $t->body))
                       (#t (ast:if (ex c0) (expand:body* env c.) $*)))))))))
    (case ((length>=? 3 form)
           (define =? (make-syntax=? ctx:var env:future env))
           (expand:let/temp
             'scrutinee env (@ 1)
             (lambda (x)
               (unless (length>=? 1 (@. 2)) (error '"invalid case:" form))
               (let loop ((c* (@. 2)))
                 (cond
                   ((null? c*) ($error (ast:quote '"no matching case clause:")
                                       x (ast:quote form)))
                   ((not (length>=? 2 (car c*))) (error '"invalid case:" form))
                   ((and (=? 'else (caar c*)) (length=? 1 c*))
                    (expand:body* env (cdar c*)))
                   ((list? (caar c*))
                    (ast:if
                      (foldr (lambda (d r) (ast:if ($equal? x d) $true r))
                             $false (map ast:quote (caar c*)))
                      (expand:body* env (cdar c*)) (loop (cdr c*))))
                   (#t (error '"invalid case:" form))))))))
    (quasiquote
      ((length=? 2 form)
       (define (tag t e) ($cons (ast:quote t) ($cons e (ast:quote '()))))
       (define =? (make-syntax=? ctx:var env:future env))
       (define (t? tag d) (and (length=? 2 d) (=? tag (car d))))
       (let loop ((level 0) (qqf (@ 1)))
         (cond ((t? 'quasiquote qqf)
                (tag 'quasiquote (loop (+ level 1) (cadr qqf))))
               ((t? 'unquote qqf)
                (if (= 0 level) (ex (cadr qqf))
                  (tag 'unquote (loop (- level 1) (cadr qqf)))))
               ((and (pair? qqf) (t? 'unquote-splicing (car qqf)))
                (define qqd (loop level (cdr qqf)))
                (if (= 0 level) ($append (ex (cadar qqf)) qqd)
                  ($cons (tag 'unquote-splicing
                              (loop (- level 1) (cadar qqf))) qqd)))
               ((vector? qqf) ($list->vector (loop level (vector->list qqf))))
               ((pair? qqf)   ($cons (loop level (car qqf))
                                     (loop level (cdr qqf))))
               ((ormap (lambda (t) (=? t qqf))
                       '(quasiquote unquote unquote-splicing))
                (error '"malformed quasiquote:" qqf form))
               (#t (ast:quote qqf))))))
    (match/=? ((length>=? 4 form)
               (parse:match/=? env (@ 1) (@ 2) (@. 3) form)))
    (match ((length>=? 3 form)
            (parse:match/=? env (expander (lambda _ $var:equal?))
                            (@ 1) (@. 2) form)))))

(append
  '(begin
     (define (parse:match/=? env =?-e scrutinee body* full-form)
       (define (parse:clause $=? $x env pat rhs $k-fail)
         (define $fail ($apply* $k-fail '()))
         (define ($try $c $k) (ast:if $c $k $fail))
         (define ($succeed env)
           (define =? (make-syntax=? ctx:var env:future env))
           (cond ((and (pair? rhs) (length>=? 1 (car rhs))
                       (=? 'guard (caar rhs)))
                  ($try (expand:and* env (cdar rhs))
                        (expand:body* env (cdr rhs))))
                 (#t (expand:body* env rhs))))
         (define (tqq datum) (list 'quasiquote datum))
         (let loop (($x $x) (p pat) (env env) ($succeed $succeed))
           (define ($try-vec pat)
             (define (k $x) (loop $x pat env $succeed))
             ($try ($vector? $x) ($let/temp 'x ($vector->list $x) k)))
           (let retry ((p p))
             (define (? tag) (equal? (car p) tag))
             (define (?= n tag) (and (? tag) (length=? n p)))
             (define (?>= n tag) (and (? tag) (length>=? n p)))
             (cond
               ((equal? p '_)                 ($succeed env))
               ((name? p)                     (retry (list 'var p)))
               ((or (boolean? p) (number? p)) (retry (list 'quote p)))
               ((not (length>=? 1 p)) (error '"invalid pattern:" pat p))
               ((and (?= 2 'var) (name? (cadr p)))
                ($let (list (cadr p)) (list $x)
                      (lambda (b*) ($succeed (env-extend*/var env b*)))))
               ((?= 2 'quote)
                ($try ($apply* $=? (list $x (ast:quote (cadr p))))
                      ($succeed env)))
               ((?= 3 'cons)
                (define ($k env)
                  ($let/temp 'x-cdr ($cdr $x)
                             (lambda ($x) (loop $x (caddr p) env $succeed))))
                ($try ($pair? $x) ($let/temp 'x-car ($car $x)
                                             (lambda ($x)
                                               (loop $x (cadr p) env $k)))))
               ((?=  2 'list*) (retry (cadr p)))
               ((?>= 3 'list*) (retry (list 'cons (cadr p)
                                            (cons 'list* (cddr p)))))
               ((? 'list)   (retry (cons 'list* (append (cdr p) (list ''())))))
               ((? 'vector) ($try-vec (cons 'list (cdr p))))
               ((?= 2 'quasiquote)
                (define qq (cadr p))
                (cond ((vector? qq) ($try-vec (tqq (vector->list qq))))
                      ((and (length=? 1 qq) (length=? 2 (car qq))
                            (equal? (caar qq) 'unquote-splicing)
                            (name? (cadar qq)))
                       ($try ($list? $x) (retry (cadar qq))))
                      ((and (length=? 2 qq) (equal? (car qq) 'unquote))
                       (retry (cadr qq)))
                      ((pair? qq)
                       (retry (list 'cons (tqq (car qq)) (tqq (cdr qq)))))
                      ((ormap (lambda (d) (equal? qq d))
                              '(quasiquote unquote unquote-splicing))
                       (error '"bad quasiquote keyword:" qq pat))
                      (#t (retry (list 'quote qq)))))
               (#t (error '"invalid pattern:" pat p))))))
       (define (parse:clauses $=? env c*)
         ($lambda/temp
           'scrutinee
           (lambda ($x)
             (foldr
               (lambda (c $fail)
                 (cond ((length>=? 2 c)
                        ($let/temp
                          'k-fail ($thunk $fail)
                          (lambda ($kf) (parse:clause
                                          $=? $x env (car c) (cdr c) $kf))))
                       (#t (error '"invalid match clause:" c))))
               ($error (ast:quote '"no matching clause:")
                       $x (ast:quote full-form)) c*))))
       (expand:let/temp
         '=? env =?-e
         (lambda ($=?)
           ($apply*
             (cond ((name? (car body*))
                    (define name (car body*))
                    (define (e env) (parse:clauses $=? env (cdr body*)))
                    (expand:letrec env (list name) (list (expander e))
                                   (lambda (env) (parse:var env name))))
                   (#t (parse:clauses $=? env body*)))
             (list (expand env scrutinee)))))))

  (list (list 'define 'env:future
              (list 'env-extend*/syntax
                    'env:base 'ctx:op
                    (parser-descs->b* 'env:future parsers:future))))

 '((define ($equal? a b)     ($apply* $var:equal? (list a b)))
   (define ($list? x)        ($apply* $var:list?  (list x)))
   (define ($append a b)     ($apply* $var:append (list a b)))
   (define ($list->vector x) ($apply* $var:list->vector (list x)))
   (define ($vector->list x) ($apply* $var:vector->list (list x)))

   (define $var:equal?       (parse:var env:future 'equal?))
   (define $var:list?        (parse:var env:future 'list?))
   (define $var:append       (parse:var env:future 'append))
   (define $var:list->vector (parse:var env:future 'list->vector))
   (define $var:vector->list (parse:var env:future 'vector->list))))
)

(define (ev form) (eval/ast (nscheme:expand
                              (expander (lambda (_)
                                          (expand env:future form))))))

(define (test! test)
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
