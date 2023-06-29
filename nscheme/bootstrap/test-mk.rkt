#lang racket/base
(require "../platform/racket/nscheme.rkt" racket/include racket/match racket/pretty racket/splicing)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/boot/record.scm")
(include "../include/boot/error.scm")
(include "../include/base/misc.scm")
(include "../include/base/compare.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/string.scm")

(include "../include/syntax.scm")
(include "../include/ast.scm")
(include "../include/parse.scm")
(include "../include/primitive.scm")
(include "../include/minimal.scm")
(include "../include/match.scm")

(include "../include/extended.scm")

;; TODO: all remaining compiler definitions should be included here, replacing ast-eval.rkt:
(require "ast-eval.rkt")

(define env.base (env-compose.match (env-compose env.primitive env.minimal)))

(define def*.microkanren
  '(;;;;;;;;;;;;;;;;;
    ;;; Utilities ;;;
    ;;;;;;;;;;;;;;;;;

    (define (not x) (if x #f #t))

    (define (map f x*)
      (let loop ((x* x*))
        (match x*
          ('()         '())
          ((cons x x*) (cons (f x) (loop x*))))))

    (define (assoc/? ? kv*)
      (let loop ((kv* kv*))
        (match kv*
          ('()                            #f)
          ((cons (and (cons k v) kv) kv*) (if (? k) kv (loop kv*))))))

    ;;;;;;;;;;;;;;;;;;;
    ;;; microKanren ;;;
    ;;;;;;;;;;;;;;;;;;;

    (define (var   id)  (vector id))
    (define (var?  x)   (vector? x))
    (define (var=? a b) (eqv? (vector-ref a 0) (vector-ref b 0)))
    (define var.initial (var -1))

    (define sub.empty '())

    (define (walk t sub)
      (let ((xt (and (var? t) (assoc/? (lambda (x) (var=? t x)) sub))))
        (if xt (walk (cdr xt) sub) t)))

    (define (walk* t sub)
      (match (walk t sub)
        ((cons u v) (cons (walk* u sub) (walk* v sub)))
        (t          t)))

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
    (define (reify t st) (walk* t (state-sub st)))))

(define $mk
  (let ((env.microkanren (env:parse-begin-definition env.base def*.microkanren)))
    (lambda (name) (parse-expression env.microkanren name))))

(define $==          ($mk '==))
(define $disj        ($mk 'disj))
(define $conj        ($mk 'conj))
(define $pause       ($mk 'pause))
(define $call/fresh  ($mk 'call/fresh))
(define $var.initial ($mk 'var.initial))
(define $state.empty ($mk 'state.empty))
(define $s-take      ($mk 's-take))
(define $reify       ($mk 'reify))
(define $map         ($mk 'map))

(define ($run env $count param ^body)
  (let ((param* (if (identifier? param) (list param) (syntax->list param))))
    ($call $map (ast:lambda #f '(st) ($call $reify $var.initial ($ref 'st)))
           ($call $s-take $count
                  ($call $pause $state.empty
                         ($fresh env param*
                                 (lambda (env . addr*)
                                   (let (($x* (map $ref addr*)))
                                     ($call $conj
                                            ($call $== $var.initial (if (identifier? param)
                                                                        (car $x*)
                                                                        (apply $list $x*)))
                                            (apply ^body env $x*))))))))))

(define ($fresh env param* ^body)
  (parse-param* param*)
  (let loop ((env env) (param* param*) (raddr* '()))
    (cond
      ((null? param*) (apply ^body env (reverse raddr*)))
      (else ($call $call/fresh
                   ($lambda env (list (car param*))
                            (lambda (env addr) (loop env (cdr param*) (cons addr raddr*)))))))))

(define ($conj* $g*)
  (if (null? $g*)
      ($call $== ($quote #t) ($quote #t))
      (let loop (($g (car $g*)) ($g* (cdr $g*)))
        (cond ((null? $g*) $g)
              (else        (loop ($call $conj $g (car $g*)) (cdr $g*)))))))

(define ($disj* $g*)
  (if (null? $g*)
      ($call $== ($quote #t) ($quote #f))
      (let loop (($g (car $g*)) ($g* (cdr $g*)))
        (cond ((null? $g*) $g)
              (else        ($call $disj $g (loop (car $g*) (cdr $g*))))))))

(define (parse-run* env param . fm*) (apply parse-run env #f param fm*))

(define (parse-run env count param . fm*)
  ($run env (parse-expression env count) param
        (lambda (env . _) ($conj* (parse-expression* env fm*)))))

(define (parse-conde env . stx*.fm*)
  ($disj* (map (lambda (stx.fm*) ($conj* (parse-expression* env (syntax->list stx.fm*))))
               stx*.fm*)))

(define (parse-fresh env param* . fm*)
  ($fresh env param* (lambda (env . _) ($conj* (parse-expression* env fm*)))))

(define (env-compose.minikanren env)
  (let ((env.scope (make-env))
        (b*.expr-op
          (list
            (cons 'fresh (expression-operator-parser parse-fresh 2 #f))
            (cons 'conde (expression-operator-parser parse-conde 1 #f))
            (cons 'run   (expression-operator-parser parse-run   3 #f))
            (cons 'run*  (expression-operator-parser parse-run*  2 #f)))))
    (for-each (lambda (id op) (env-bind! env.scope id vocab.expression-operator op))
              (map car b*.expr-op) (map cdr b*.expr-op))
    (env-bind! env.scope '== vocab.expression (lambda (_ __) $==))
    (env-freeze! env.scope)
    (env-compose env env.scope)))

(define env.mk (env-compose.minikanren env.base))

(define (test-mk* body)
  (let ((ast (time (parse-body env.mk body))))
    (pretty-write (time (ast-eval ast)))))

(define (test-mk expr)
  (newline)
  (pretty-write expr)
  (test-mk* (list expr)))

(test-mk '(run 1 q (== 5 5)))
(test-mk '(run 1 q (== 5 3)))
(test-mk '(run 1 q (== 5 q)))

(test-mk '(run* q (== 5 5)))
(test-mk '(run* q (== 5 3)))
(test-mk '(run* q (== 5 q)))

(define def.appendo
  '(define (appendo x* y x*y)
     (conde
       ((== x* '()) (== x*y y))
       ((fresh (x z* z*y)
          (== x*  (cons x z*))
          (== x*y (cons x z*y))
          (appendo z* y z*y))))))

(define (test-appendo expr)
  (newline)
  (pretty-write expr)
  (test-mk* (list def.appendo expr)))

(test-appendo '(run 1 q (appendo '(1 2 3) '(4 5) q)))
(test-appendo '(run* (x* y*) (appendo x* y* '(1 2 3 4 5))))

(define def*.evalo
  '((define (eval-expo expr env value)
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
    (define (lookupo index env value)
      (fresh (arg e*)
        (== `(,arg . ,e*) env)
        (conde
          ((== '() index) (== arg value))
          ((fresh (i* a d)
             (== `(s . ,i*) index)
             (== `(,a . ,d) e*)
             (lookupo i* e* value))))))
    (define (eval-listo e* env value)
      (conde
        ((== '() e*) (== '() value))
        ((fresh (ea ed va vd)
           (== `(,ea . ,ed) e*)
           (== `(,va . ,vd) value)
           (eval-expo ea env va)
           (eval-listo ed env vd)))))
    (define (evalo expr value) (eval-expo expr '() value))))

(define (test-evalo expr)
  (newline)
  (pretty-write expr)
  (test-mk* (append def*.evalo (list expr))))

;; ~2 seconds
(test-evalo '(run 1 e (evalo e e)))