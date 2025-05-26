(begin-meta
  (splicing-local
    (;;;;;;;;;;;;;;;;;;;
     ;;; microKanren ;;;
     ;;;;;;;;;;;;;;;;;;;
     (define (var   id)  (vector id))
     (define (var?  x)   (vector? x))
     (define (var=? a b) (eqv? (vector-ref a 0) (vector-ref b 0)))
     (define var.initial (var -1))

     (define sub.empty '())

     (define (walk t sub)
       (let ((xt (and (var? t) (assp (lambda (x) (var=? t x)) sub))))
         (if xt (walk (cdr xt) sub) t)))

     (define (walk* t sub)
       (let ((t (walk t sub)))
         (if (pair? t)
             (cons (walk* (car t) sub) (walk* (cdr t) sub))
             t)))

     (define (occurs? x t sub)
       (let loop ((t t))
         (cond ((pair? t) (or (loop (walk (car t) sub))
                              (loop (walk (cdr t) sub))))
               ((var?  t) (var=? x t))
               (else      #f))))

     (define (extend-sub x t sub)
       (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

     (define (make-state sub id.next)   (cons sub id.next))
     (define (state-sub         st)     (car st))
     (define (state-next-id     st)     (cdr st))
     (define (set-state-next-id st id)  (make-state (state-sub st) id))
     (define (set-state-sub     st sub) (make-state sub (state-next-id st)))

     (define state.empty '(() . 0))

     (define (call/fresh var->g)
       (lambda (st)
         (let ((id (state-next-id st)))
           ((var->g (var id)) (set-state-next-id st (+ id 1))))))

     (define (assign x t st)
       (let ((sub.new (extend-sub x t (state-sub st))))
         (and sub.new (set-state-sub st sub.new))))

     (define (unify u v st)
       (let* ((sub (state-sub st))
              (u   (walk u sub))
              (v   (walk v sub)))
         ;; TODO: it might also be useful to allow any procedure values to unify.
         (cond ((eqv? u v) st)
               ((var? u)   (if (and (var? v) (var=? u v)) st (assign u v st)))
               ((var? v)   (assign v u st))
               ((pair? u)  (and (pair? v) (let ((st (unify (car u) (car v) st)))
                                            (and st (unify (cdr u) (cdr v) st)))))
               (else       #f))))

     (define (state->stream st) (and st (cons st #f)))

     (define (mature? s) (or (not s) (pair? s)))
     (define (mature  s) (if (mature? s) s (mature (s))))
     (define (step    s) (if (mature? s) s (s)))

     (define (s-take n s)
       (cond ((eqv? 0 n) '())
             (else (let ((s (mature s)))
                     (cond ((pair? s) (cons (car s) (s-take (and n (- n 1)) (cdr s))))
                           (else      '()))))))

     (define (pause st g) (lambda () (g st)))

     (define (mplus s1 s2)
       (let ((s1 (step s1)))
         (cond ((not   s1) s2)
               ((pair? s1) (cons (car s1) (lambda () (mplus s2 (cdr s1)))))
               (else       (lambda () (mplus s2 s1))))))

     (define (bind s g)
       (let ((s (step s)))
         (cond ((not   s) #f)
               ((pair? s) (mplus (pause (car s) g)
                                 (lambda () (bind (cdr s) g))))
               (else      (lambda () (bind s g))))))

     (define (disj   g1 g2) (lambda (st) (mplus (pause st g1) (pause st g2))))
     (define (conj   g1 g2) (lambda (st) (bind (pause st g1) g2)))
     (define (relate thunk) (lambda (st) (pause st (thunk))))
     (define (==     u v)   (lambda (st) (state->stream (unify u v st))))

     ;; TODO: improve reification
     (define (reify t st) (walk* t (state-sub st))))

    ;;;;;;;;;;;;;;;;;;
    ;;; miniKanren ;;;
    ;;;;;;;;;;;;;;;;;;
    (define vocab.formula 'formula)
    (define (parse-formula* env stx*) (map (lambda (stx) (parse-formula env stx)) stx*))
    (define (parse-formula env stx)
      (define (^default) (raise-parse-error (list vocab.formula "not a formula") stx))
      ($source (((vocabulary-parser vocab.formula ^default) env stx) env stx) stx))
    (splicing-let (($$==         ($quote ==))
                   ($$disj       ($quote disj))
                   ($$conj       ($quote conj))
                   ($$pause      ($quote pause))
                   ($$call/fresh ($quote call/fresh))
                   ($$s-take     ($quote s-take))
                   ($$reify      ($quote reify))
                   ($$map        ($quote map)))
      (define ($==         a b)  ($call $$==         a b))
      (define ($disj       a b)  ($call $$disj       a b))
      (define ($conj       a b)  ($call $$conj       a b))
      (define ($pause      a b)  ($call $$pause      a b))
      (define ($call/fresh x->g) ($call $$call/fresh x->g))
      (define ($s-take     n s)  ($call $$s-take     n s))
      (define ($reify      x st) ($call $$reify      x st))
      (define ($map        f x*) ($call $$map        f x*)))
    (define $var.initial ($quote var.initial))
    (define $state.empty ($quote state.empty))

    (define ($run env $count param ^body)
      ($map ($lambda '(st) (lambda ($st) ($reify $var.initial $st)))
            ($s-take $count
                     ($pause $state.empty
                             (if (identifier? param)
                                 ($let/env env (list param) (list $var.initial) ^body)
                                 (let ((param* (syntax->list param)))
                                   (parse-param* param*)
                                   ($fresh env param*
                                           (lambda (env)
                                             (let (($x* (parse-expression* env param*)))
                                               ($conj ($== $var.initial (apply $list $x*))
                                                      (^body env)))))))))))

    (define ($fresh env param* ^body)
      (let loop ((env env) (param* param*))
        (if (null? param*)
            (^body env)
            ($call/fresh ($lambda/env env (list (car param*))
                                      (lambda (env) (loop env (cdr param*))))))))

    (define ($conj* $g*)
      (if (null? $g*)
          ($== ($quote #t) ($quote #t))
          (let loop (($g (car $g*)) ($g* (cdr $g*)))
            (if (null? $g*) $g (loop ($conj $g (car $g*)) (cdr $g*))))))

    (define ($disj* $g*)
      (if (null? $g*)
          ($== ($quote #t) ($quote #f))
          (let loop (($g (car $g*)) ($g* (cdr $g*)))
            (if (null? $g*) $g ($disj $g (loop (car $g*) (cdr $g*)))))))

    (define (parse-run* env param . fm*) (apply parse-run env #f param fm*))

    (define (parse-run env count param . fm*)
      ($run env (parse-expression env count) param (lambda (env) ($conj* (parse-formula* env fm*)))))

    (define (parse-conde env . stx*.fm*)
      ($disj* (map (lambda (stx.fm*) ($conj* (parse-formula* env (syntax->list stx.fm*))))
                   stx*.fm*)))

    (define (parse-fresh env param* . fm*)
      (let ((param* (syntax->list param*)))
        (parse-param* param*)
        ($fresh env param* (lambda (env) ($conj* (parse-formula* env fm*))))))

    (define (parse-== env lhs rhs) ($== (parse-expression env lhs) (parse-expression env rhs)))

    (define (parse-define-relation env stx.rhead . fm*)
      (let ((rhead (syntax->list stx.rhead)))
        (when (null? rhead) (raise-parse-error "empty relation head" stx.rhead))
        (let ((rid (car rhead)) (param* (cdr rhead)))
          (parse-identifier rid)
          (parse-param* param*)
          (($d:define/vocabulary
             vocab.formula
             (lambda ($rel)
               (lambda (env stx)
                 (let ((arg* (cdr (syntax->list stx))))
                   (unless (= (length param*) (length arg*))
                     (raise-parse-error
                       (list "relation arity mismatch"
                             'relation rhead 'given (length arg*) 'expected (length param*))
                       stx))
                   ($call* $rel (parse-expression* env arg*))))))
           env rid
           (let ((env (env-read-only env)))
             (lambda () ($lambda/env env param* (lambda (env)
                                                  ($conj* (parse-formula* env fm*))))))))))))

(define-vocabulary define-relation
  vocab.definition (operator-parser parse-define-relation 2 #f))
(define-vocabulary defrel
  vocab.definition (operator-parser parse-define-relation 2 #f))
(define-vocabulary run
  vocab.expression (operator-parser parse-run 3 #f))
(define-vocabulary run*
  vocab.expression (operator-parser parse-run* 2 #f))
(define-vocabulary fresh
  vocab.formula (operator-parser parse-fresh 2 #f))
(define-vocabulary conde
  vocab.formula (operator-parser parse-conde 1 #f))
(define-vocabulary ==
  vocab.formula (operator-parser parse-== 2 2))

(define-vocabulary test
  'expression
  (lambda (env stx)
    (apply $begin
           (map (lambda (expr)
                  ($begin
                    ($call ($quote pretty-write) ($quote (syntax->datum expr)))
                    ($call ($quote pretty-write)
                           ($call ($quote with-milliseconds)
                                  ($quote displayln)
                                  ($lambda '() (lambda () (parse-expression env expr)))))
                    ($call ($quote newline))))
                (cdr (syntax->list stx))))))

(test
  (run 1 q (== 5 5))
  (run 1 q (== 5 3))
  (run 1 q (== 5 q))
  (run* q (== 5 5))
  (run* q (== 5 3))
  (run* q (== 5 q)))

(define-relation (appendo x* y x*y)
  (conde
    ((== x* '()) (== x*y y))
    ((fresh (x z* z*y)
       (== x*  (cons x z*))
       (== x*y (cons x z*y))
       (appendo z* y z*y)))))

(test
  (run 1 q (appendo '(1 2 3) '(4 5) q))
  (run* (x* y*) (appendo x* y* '(1 2 3 4 5))))

(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
    ((fresh (body)
       (== `(lambda ,body) expr)      ;; expr is a procedure definition
       (== `(closure ,body ,env) value)))
    ;; If this is before lambda, quoted closures become likely.
    ((== `(quote ,value) expr))       ;; expr is a literal constant
    ((fresh (a*)
       (== `(list . ,a*) expr)        ;; expr is a list operation
       (eval-listo a* env value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)        ;; expr is a cons operation
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (index)
       (== `(var ,index) expr)        ;; expr is a variable
       (lookupo index env value)))
    ;((fresh (c va vd)
    ;(== `(car ,c) expr)            ;; expr is a car operation
    ;(== va value)
    ;(eval-expo c env `(,va . ,vd))))
    ;((fresh (c va vd)
    ;(== `(cdr ,c) expr)            ;; expr is a cdr operation
    ;(== vd value)
    ;(eval-expo c env `(,va . ,vd))))
    ((fresh (rator rand arg env^ body)
       (== `(app ,rator ,rand) expr)  ;; expr is a procedure application
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `(,arg . ,env^) value)))))
(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i* a d)
         (== `(s . ,i*) index)
         (== `(,a . ,d) e*)
         (lookupo i* e* value))))))
(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))
(define-relation (evalo expr value) (eval-expo expr '() value))

(test
  (run 1 e (evalo e e)))
