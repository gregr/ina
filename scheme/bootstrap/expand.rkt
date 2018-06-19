#lang racket/base
(provide
  expand
  )

(require
  "ast.rkt"
  "data.rkt"
  "match.rkt"
  "syntax.rkt"
  "../type.rkt"
  racket/list
  racket/set
  )

(define (reverse-append xs ys)
  (if (null? xs) ys (reverse-append (cdr xs) (cons (car xs) ys))))

(define (fresh-name sym) (labeled-name sym (mvector '#())))

;;; Parameters
(define (improper-list? d)
  (and (not (null? d)) (or (not (pair? d)) (improper-list? (cdr d)))))
(define (~list->list d)
  (cond ((null? d) '())
        ((pair? d) (cons (car d) (~list->list (cdr d))))
        (else (list d))))

(define (name? n) (or (symbol? n) (labeled-name? n)))
(define (param? p) (or (not p) (name? p)))
(define (assert-param* p*)
  (when (not (andmap param? p*)) (error "invalid parameter list:" p*))
  (define n* (filter name? p*))
  (when (not (= (length n*) (set-count (list->set n*))))
    (error "parameter list contains duplicate names:" p*)))
(define (assert-binding* b*)
  (define (binding? b) (and (list? b) (= 2 (length b))
                            (or (not (car b)) (name? (car b)))))
  (when (not (and (list? b*) (andmap binding? b*)))
    (error "invalid binding list:" b*)))
(define (param*->addr* n*)
  (map (let ((label 0))
         (lambda (n) (and (name? n) (set! label (+ 1 label))
                          (labeled-name (name->symbol n) label)))) n*))

;;; Expansion
(define primitive-op-expanders
  (make-immutable-hash
    (map (lambda (po-desc)
           (define name (car po-desc))
           (define arity (length (cadr po-desc)))
           (cons name
                 (lambda (env form)
                   (match-syntax env form
                     (`(,_ . ,a*)
                       (guard (list? a*) (= arity (length a*)))
                       (ast-primitive-op
                         name (map (lambda (a) (expand/env env a)) a*)))
                     (_ (error "invalid primitive op:" name arity form))))))
         primitive-ops)))

(define ($lambda variadic? p* a* b*->body)
  (ast-lambda variadic? a* (b*->body (map cons p* a*))))
(define ($let p* a?* v* b*->body) (ast-apply* ($lambda #f p* a?* b*->body) v*))
(define ($begin body* body-final)
  (cond ((null? body*) body-final)
        (else ($let '(#f) '(#f) (list (car body*))
                    (lambda _ ($begin (cdr body*) body-final))))))
(define ($b*->body env body)
  (lambda (b*) (let ((cenv (env-extend env)))
                 (env-bind*! cenv b*) (expand/env cenv body))))

(define (form->transformer env form)
  (define n (if (pair? form) (car form) form))
  (if (closed-name? n)
    (form->transformer (closed-name-env n) (closed-name-n n))
    (env-ref-transformer env n)))

(define (expand-letrec env p* a?* v* body)
  (define ast-true (expand #t))
  (define uninitialized* (map (lambda (_) ast-true) p*))
  (define (pbody b*)
    (let* ((cenv (env-extend env))
           (_ (env-bind*! cenv b*))
           (a?* (map cdr b*))
           (e* (map (lambda (v) (expand/env cenv v)) v*)))
      ($begin (map (lambda (a? e) (if a? (ast-set! a? e) e)) a?* e*)
              (expand/env cenv body))))
  ($let p* (param*->addr* p*) uninitialized* pbody))

(define (expand form) (expand/env env-initial form))
(define (expand/env env form)
  (define (loop d) (expand/env env d))
  (define (loop-close d) (loop (syntax-close env-initial d)))
  (cond ((form->transformer env form)
         => (lambda (t) (expand/env env (t env form))))
        ((or (boolean? form) (number? form) (char? form) (string? form))
         (ast-literal form))
        ((closed-name? form)
         (expand/env (closed-name-env form) (closed-name-n form)))
        ((name? form)
         (ast-variable (env-ref-lexical env form)))
        (else (match-syntax
                env form
                (`(quote ,datum) (ast-literal datum))
                (`(set! ,name ,e)
                  (guard (or (closed-name? name) (name? name)))
                  (define addr
                    (if (closed-name? name)
                      (env-ref-lexical (closed-name-env name)
                                       (closed-name-n name))
                      (env-ref-lexical env name)))
                  (ast-set! addr (loop e)))
                (`(lambda ,~p* ,body)
                  (define p* (~list->list ~p*))
                  (assert-param* p*)
                  (define pbody ($b*->body env body))
                  ($lambda (improper-list? ~p*) p* (param*->addr* p*) pbody))

                (`(let ,nm ,b* ,bdy)
                  (guard (or (name? nm)))
                  (assert-binding* b*)
                  (define name (syntax-open nm))
                  (define p* (syntax-open (map car b*)))
                  (define v* (syntax-open (map cadr b*)))
                  (define body (syntax-open bdy))
                  (loop-close `(letrec ((,name (lambda ,p* ,body)))
                                 (,name . ,v*))))

                (`(let ,b* ,body)
                  (assert-binding* b*)
                  (define p* (map car b*))
                  (define v* (map cadr b*))
                  (define pbody ($b*->body env body))
                  ($let p* (param*->addr* p*) (map loop v*) pbody))

                (`(letrec ,b* ,body)
                  (assert-binding* b*)
                  (define p* (map car b*))
                  (define v* (map cadr b*))
                  (expand-letrec env p* (param*->addr* p*) v* body))

                (`(reset ,body) (ast-reset (loop body)))
                (`(shift ,k ,body)
                  (match (param*->addr* '(k-raw k arg))
                    ((list k-raw-addr k-addr arg-addr)
                     (define inner-body
                       ($let (list k) (list k-addr)
                             (list ($lambda
                                     #f (list arg-addr) (list arg-addr)
                                     (lambda _
                                       (ast-unshift (ast-variable k-raw-addr)
                                                    (ast-variable arg-addr)))))
                             ($b*->body env body)))
                     (ast-shift ($lambda #f (list k-raw-addr) (list k-raw-addr)
                                         (lambda _ inner-body))))))

                (`(begin ,body-first ,@body-rest)
                  (let ((body-first (loop body-first))
                        (body-rest (map loop body-rest)))
                    (define body* (reverse-append body-rest (list body-first)))
                    ($begin (reverse (cdr body*)) (car body*))))

                (`(,op-name . ,a*)
                  (guard (hash-has-key? primitive-op-expanders op-name))
                  ((hash-ref primitive-op-expanders op-name) env form))
                (`(if ,c ,t ,f) (ast-if (loop c) (loop t) (loop f)))
                (`(apply ,p ,a) (ast-apply (loop p) (loop a)))
                (`(,p . ,a*)    (guard (list? a*))
                                (ast-apply* (loop p) (map loop a*)))
                (_ (error "invalid syntax:" form)))))

  ;; TODO:
  ;; lambda body recursive definition contexts

  ;; let*
  ;; quasiquote

  ;; cond, and, or, when, unless, case, match

  ;; let-syntax, letrec-syntax
  )

(define env-initial (env-extend env-empty))
(define-syntax define-syntax-transformer*
  (syntax-rules ()
    ((_ e-local e f) '())
    ((_ e-local e f (name clause ...) rest ...)
     `((name . ,(lambda (e f)
                  (syntax-close e-local (match-syntax
                                          e f clause ...
                                          (_ (error "invalid syntax:" f))))))
       . ,(define-syntax-transformer* e-local e f rest ...)))))
(env-bind-transformer*!
  env-initial
  (define-syntax-transformer*
    env-initial env form
    (letrec* (`(,_ ,b* ,body) `(letrec ,(syntax-open b*) ,(syntax-open body))))
    ))

;; TODO: translate this.
;(define (stdlib program)
  ;(define (po->def po)
    ;(define name (car po))
    ;(define p*
      ;(map (lambda (i) (string->symbol (string-append "x" (number->string i))))
           ;(range (length (cadr po)))))
    ;`(,name (lambda ,p* (,name . ,p*))))

  ;#`(let #,(map po->def primitive-ops)
      ;(letrec #,derived-ops #,program)))
