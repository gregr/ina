#lang racket/base
(provide
  expand
  primitive-op-module
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

;;; Expansion
(define (form->transformer env form)
  (define n (if (pair? form) (car form) form))
  (if (closed-name? n)
    (form->transformer (closed-name-env n) (closed-name-n n))
    (env-ref-transformer env n)))
(define (form->parser env form)
  (define n (if (pair? form) (car form) form))
  (if (closed-name? n)
    (form->parser (closed-name-env n) (closed-name-n n))
    (env-ref-parser env n)))

(define (expand/env env form)
  (define (loop d) (expand/env env d))
  (cond ((form->transformer env form)
         => (lambda (t) (expand/env env (t env form))))
        ((form->parser env form) => (lambda (p) (p env form)))
        ((or (boolean? form) (number? form) (char? form) (string? form))
         (ast-literal form))
        ((closed-name? form)
         (expand/env (closed-name-env form) (closed-name-n form)))
        ((name? form) (ast-variable (env-ref-lexical env form)))
        (else (match-syntax
                env form
                (`(,p ,@a*) (ast-apply* (loop p) (map loop a*)))
                (_          (error "invalid syntax:" form))))))

;;; Misc
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

;;; High-level AST construction
(define ($lambda variadic? p* a* b*->body)
  (ast-lambda variadic? a* (b*->body (map cons p* a*))))
(define ($let p* a?* v* b*->body) (ast-apply* ($lambda #f p* a?* b*->body) v*))
(define ($begin body* body-final)
  (cond ((null? body*) body-final)
        (else ($let '(#f) '(#f) (list (car body*))
                    (lambda _ ($begin (cdr body*) body-final))))))

;;; Standard definitions
(define ($b*->body env body)
  (lambda (b*) (env-bind*! env b*) (expand-body* env body)))

(define (expand-letrec env p* a?* v* expand-body)
  (define ast-true (expand #t))
  (define uninitialized* (map (lambda (_) ast-true) p*))
  (define (pbody b*)
    (env-bind*! env b*)
    (let ((e* (map (lambda (v) (expand/env env v)) v*)))
      ($begin (map (lambda (a? e) (if a? (ast-set! a? e) e)) a?* e*)
              (expand-body env))))
  ($let p* a?* uninitialized* pbody))

(define (expand-body* env body)
  (define denv (env-extend env))
  (define def*&body (expand-define* denv body))  ;; list of ((p . a) . rhs)
  (when (null? def*&body) (error "body cannot be empty:" body))
  (define rd*b (reverse def*&body))
  (define def* (reverse (cdr rd*b)))
  (define final-rib (car rd*b))
  (define final (cdr final-rib))
  (when (caar final-rib) (error "body cannot end with a definition:" body))
  (let ((p* (map caar def*)) (a* (map cdar def*)) (v* (map cdr def*)))
    (expand-letrec denv p* a* v* (lambda (env) (expand/env env final))))  )

(define (expand-define* env body)
  (when (not (list? body)) (error "invalid definition body:" body))
  (let outer-loop ((top body) (pending '()))
    (match top
      (`(,original-form . ,body-rest)
        (let loop ((form original-form))
          (match-syntax
            env form
            (`(begin ,@e*) (outer-loop e* (cons body-rest pending)))
            (`(define ,n ,def-body)
              (guard (name? n))
              (define b (cons n (car (param*->addr* (list n)))))
              (env-bind*! env (list b))
              (cons (cons b def-body) (outer-loop body-rest pending)))
            (`(define (,n . ,p*) . ,def-body)
              (loop (syntax-close
                      env-initial `(define ,(syntax-open n)
                                     (lambda ,(syntax-open p*)
                                       . ,(syntax-open def-body))))))
            (`(begin . ,_) (error "invalid begin syntax:" form))
            (`(define . ,_) (error "invalid define syntax:" form))
            (_ (cond ((form->transformer env form)
                      => (lambda (t) (loop (t env form))))
                     (else (cons (cons (cons #f #f) original-form)
                                 (outer-loop body-rest pending))))))))
      ('() (match pending
             (`(,top . ,pending) (outer-loop top pending))
             ('() '()))))))

(define (expand form) (expand/env env-initial form))

(define env-initial (env-extend env-empty))
(define-syntax define-syntax-parser*
  (syntax-rules ()
    ((_ e f (common* ...)) '())
    ((_ e f (common* ...) (name (c* ...)) rest ...)
     `((name . ,(lambda (e f) common* ...
                  (match-syntax e f c* ... (_ (error "invalid syntax:" f)))))
       . ,(define-syntax-parser* e f (common* ...) rest ...)))))
(define-syntax define-syntax-transformer*
  (syntax-rules ()
    ((_ e-local e f) '())
    ((_ e-local e f (name (c* ...)) rest ...)
     `((name . ,(lambda (e f)
                  (syntax-close e-local (match-syntax
                                          e f c* ...
                                          (_ (error "invalid syntax:" f))))))
       . ,(define-syntax-transformer* e-local e f rest ...)))))

(env-bind-parser*!
  env-initial
  (define-syntax-parser*
    env form
    ((define (loop d) (expand/env env d))
     (define (loop-close d) (loop (syntax-close env-initial d))))
    (apply ((`(apply ,p ,a)  (ast-apply (loop p) (loop a)))))
    (quote ((`(quote ,datum) (ast-literal datum))))
    (if    ((`(if ,c ,t ,f) (ast-if (loop c) (loop t) (loop f)))))
    (set! ((`(set! ,name ,e)
             (guard (or (closed-name? name) (name? name)))
             (define addr (if (closed-name? name)
                            (env-ref-lexical (closed-name-env name)
                                             (closed-name-n name))
                            (env-ref-lexical env name)))
             (ast-set! addr (loop e)))))
    (lambda ((`(lambda ,~p* ,@body)
               (define p* (~list->list ~p*))
               (assert-param* p*)
               (define pbody ($b*->body (env-extend env) body))
               ($lambda (improper-list? ~p*) p* (param*->addr* p*) pbody))))
    (letrec ((`(letrec ,b* ,@body)
               (assert-binding* b*)
               (define p* (map car b*))
               (define v* (map cadr b*))
               (expand-letrec (env-extend env) p* (param*->addr* p*) v*
                              (lambda (env) (expand-body* env body))))))
    (let ((`(let ,nm ,b* . ,bdy)
            (guard (or (name? nm)))
            (assert-binding* b*)
            (define name (syntax-open nm))
            (define p* (syntax-open (map car b*)))
            (define v* (syntax-open (map cadr b*)))
            (define body (syntax-open bdy))
            (loop-close `(letrec ((,name (lambda ,p* . ,body)))
                           (,name . ,v*))))
          (`(let ,b* ,@body)
            (assert-binding* b*)
            (define p* (map car b*))
            (define v* (map cadr b*))
            (define pbody ($b*->body (env-extend env) body))
            ($let p* (param*->addr* p*) (map loop v*) pbody))))
    (begin ((`(begin ,body-first ,@body-rest)
              (let ((body-first (loop body-first))
                    (body-rest (map loop body-rest)))
                (define body* (reverse-append body-rest (list body-first)))
                ($begin (reverse (cdr body*)) (car body*))))))
    (reset ((`(reset ,@body) (ast-reset (expand-body* env body)))))
    (shift ((`(shift ,k ,@body)
              (match (param*->addr* '(k-raw k arg))
                ((list k-raw-addr k-addr arg-addr)
                 (define inner-body
                   ($let (list k) (list k-addr)
                         (list ($lambda
                                 #f (list arg-addr) (list arg-addr)
                                 (lambda _
                                   (ast-unshift (ast-variable k-raw-addr)
                                                (ast-variable arg-addr)))))
                         ($b*->body (env-extend env) body)))
                 (ast-shift ($lambda #f (list k-raw-addr) (list k-raw-addr)
                                     (lambda _ inner-body))))))))
  ;; TODO:
  ;; quasiquote
  ;; case, match
  ;; let-syntax, letrec-syntax
    ))

(env-bind-transformer*!
  env-initial
  (define-syntax-transformer*
    env-initial env form
    (letrec* ((`(letrec* ,b* . ,body)
                `(letrec ,(syntax-open b*) . ,(syntax-open body)))))
    (let* ((`(let* () . ,body) `(let () . ,(syntax-open body)))
           (`(let* (,b . ,b*) . ,body)
             `(let (,(syntax-open b))
                (let* ,(syntax-open b*) . ,(syntax-open body))))))
    (cond ((`(cond) '((quote error:cond:no-matching-clause)))
           (`(cond (else ,@body)) `(let () . ,(syntax-open body)))
           (`(cond (,e) . ,cs)
             (define t (syntax-open (fresh-name 't)))
             `(let ((,t ,(syntax-open e)))
                (if ,t ,t (cond . ,(syntax-open cs)))))
           (`(cond (,e => ,proc) . ,cs)
             (define t (syntax-open (fresh-name 't)))
             `(let ((,t ,(syntax-open e)))
                (if ,t (,(syntax-open proc) ,t) (cond . ,(syntax-open cs)))))
           (`(cond (,e ,@body) . ,cs)
             (define t (syntax-open (fresh-name 't)))
             `(let ((,t ,(syntax-open e)))
                (if ,t (let () . ,(syntax-open body))
                  (cond . ,(syntax-open cs)))))))
    (and ((`(and) #t)
          (`(and ,e) (syntax-open e))
          (`(and ,e . ,e*) `(if ,(syntax-open e)
                              (and . ,(syntax-open e*)) #f))))
    (or ((`(or) #f)
         (`(or ,e) (syntax-open e))
         (`(or ,e . ,e*)
           (define t (syntax-open (fresh-name 't)))
           `(let ((,t ,(syntax-open e)))
              (if ,t ,t (or . ,(syntax-open e*)))))))
    (when ((`(when ,c . ,body)
             `(if ,(syntax-open c) (let () . ,(syntax-open body)) #t))))
    (unless ((`(unless ,c . ,body)
               `(if ,(syntax-open c) #t (let () . ,(syntax-open body))))))
    ))

(define env-primitive (env-extend env-initial))
(env-bind-parser*!
  env-primitive
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arity (length (cadr po-desc)))
         (cons name
               (lambda (env form)
                 (match-syntax
                   env form
                   (`(,_ . ,a*)
                     (guard (list? a*) (= arity (length a*)))
                     (ast-primitive-op
                       name (map (lambda (a) (expand/env env a)) a*)))
                   (_ (error "invalid primitive op:" name arity form))))))
       primitive-ops))

(define primitive-op-module
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define addr (car (param*->addr* (list name))))
         (env-bind*! env-initial (list (cons name addr)))
         (define p*
           (map (lambda (i)
                  (string->symbol (string-append "x" (number->string i))))
                (range (length (cadr po-desc)))))
         `(,addr . ,(expand/env env-primitive `(lambda ,p* (,name . ,p*)))))
       primitive-ops))
