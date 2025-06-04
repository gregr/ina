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
      ($source ((vocabulary-parser vocab.formula ^default) env stx) stx))
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

    (define ($run0 env $count param ^body)
      ($map ($lambda '(st) (lambda ($st) ($reify $var.initial $st)))
            ($s-take $count
                     ($pause $state.empty
                             ($let/env env (list param) (list $var.initial) ^body)))))

    (define ($fresh0 env param ^body) ($call/fresh ($lambda/env env (list param) ^body)))

    (define (parse-run0 env count param fm)
      ($run0 env (parse-expression env count) param (lambda (env) (parse-formula env fm))))

    (define (parse-fresh0 env param fm) ($fresh0 env param (lambda (env) (parse-formula env fm))))

    (define (parse-conj env a b) ($conj (parse-formula env a) (parse-formula env b)))
    (define (parse-disj env a b) ($disj (parse-formula env a) (parse-formula env b)))

    (define (parse-== env lhs rhs) ($== (parse-expression env lhs) (parse-expression env rhs)))

    (define (parse-define-relation0 env stx.rhead fm)
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
             (lambda () ($lambda/env env param* (lambda (env) (parse-formula env fm)))))))))))

(define-vocabulary-syntax-binder define-formula-syntax define-in-vocabulary vocab.formula #f parse-formula)

(define-in-vocabulary ==
  vocab.formula (operator-parser parse-== 2 2))

(splicing-local
  ((define-in-vocabulary define-relation0
     vocab.definition (operator-parser parse-define-relation0 2 2))
   (define-in-vocabulary run0
     vocab.expression (operator-parser parse-run0 3 3))
   (define-in-vocabulary fresh0
     vocab.formula (operator-parser parse-fresh0 2 2))
   (define-in-vocabulary conj
     vocab.formula (operator-parser parse-conj 2 2))
   (define-in-vocabulary disj
     vocab.formula (operator-parser parse-disj 2 2)))

  (define-syntax (define-relation stx)
    (match (syntax->list stx)
      ((cons* _ rhead fm*)
       (quasiquote-syntax (define-relation0 #,rhead (fresh () . #,fm*))))))

  (define-syntax (run stx)
    (match (syntax->list stx)
      ((cons* _ count param body*)
       (if (identifier? param)
           (quasiquote-syntax
             (run0 #,count #,param (fresh () . #,body*)))
           (quasiquote-syntax
             (run0 #,count q (fresh #,param (== q (list . #,param)) . #,body*)))))))

  (define-syntax (run* stx)
    (match (syntax->list stx)
      ((cons* _ param body*)
       (quasiquote-syntax (run #f #,param . #,body*)))))

  (define-formula-syntax (fresh stx)
    (match (syntax->list stx)
      ((cons* _ param* body)
       (match (cons (syntax->list param*) body)
         ((list  '() fm)          fm)
         ((list  '() fm1 fm2)     (quasiquote-syntax (conj #,fm1 #,fm2)))
         ((cons* '() fm1 fm2 fm*) (quasiquote-syntax (fresh () (conj #,fm1 #,fm2) . #,fm*)))
         ((cons* x*  fm*)         (match (syntax->list x*)
                                    ((cons* x x*)
                                     (quasiquote-syntax (fresh0 #,x (fresh #,x* . #,fm*))))))))))

  (define-formula-syntax (conde stx)
    (match (syntax->list stx)
      ((list  _ fm*)      (quasiquote-syntax (fresh () . #,fm*)))
      ((cons* _ fm* fm**)
       (quasiquote-syntax (disj (fresh () . #,fm*) (conde . #,fm**)))))))

(define-syntax (defrel stx)
  (match (syntax->list stx)
    ((cons* _ stx*)
     (quasiquote-syntax (define-relation . #,stx*)))))

(define-syntax (test stx)
  (match (syntax->list stx)
    ((list _) (quote-syntax (values)))
    ((cons* _ expr expr*)
     (quasiquote-syntax
       (begin (pretty-write '#,expr)
              (pretty-write (with-milliseconds displayln (lambda () #,expr)))
              (newline)
              (test . #,expr*))))))

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
