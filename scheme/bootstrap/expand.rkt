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

(define ($lambda variadic? p* a?* b*->body)
  (define a* (filter-not not a?*))
  (ast-lambda variadic? a?* (b*->body (map cons (filter-not not p*) a*))))
(define ($let p* a?* v* b*->body) (ast-apply* ($lambda #f p* a?* b*->body) v*))
(define ($begin body* body-final)
  (cond ((null? body*) body-final)
        (else ($let '(#f) '(#f) (list (car body*))
                    (lambda _ ($begin (cdr body*) body-final))))))
(define ($b*->body env body)
  (lambda (b*) (let ((cenv (env-extend env)))
                 (env-bind*! cenv b*) (expand/env cenv body))))

(define (form->transformer env form)
  (define b (syntax-resolve env (if (pair? form) (car form) form)))
  (and (procedure? b) b))

(define (expand-once env form)
  (define (loop d) (expand/env env d))
  (cond ((form->transformer env form) => (lambda (t) (t env form)))
        ((or (boolean? form) (number? form) (char? form) (string? form))
         (ast-literal form))
        ((closed-name? form)
         (expand-once (closed-name-env form) (closed-name-n form)))
        ((name? form)
         (ast-variable (env-ref-lexical env form)))
        (else (match-syntax
                env form
                (`(quote ,datum) (ast-literal datum))
                (`(set! ,name ,e)
                  (guard (or (closed-name? name) (name? name)))
                  (define addr
                    (if (closed-name? name)
                      (env-ref-lexical (closed-name-env) (closed-name-n name))
                      (env-ref-lexical env name)))
                  (ast-set! addr (loop e)))
                (`(lambda ,~p* ,body)
                  (define p* (~list->list ~p*))
                  (assert-param* p*)
                  (define pbody ($b*->body env body))
                  ($lambda (improper-list? ~p*) p* (param*->addr* p*) pbody))

                ;(`(let ,name ,b* ,body) (guard (name? name))
                                        ;(assert-binding* b*)

                  ;)

                (`(let ,b* ,body)
                  (assert-binding* b*)
                  (define p* (map car b*))
                  (define v* (map cadr b*))
                  (define pbody ($b*->body env body))
                  ($let p* (param*->addr* p*) (map loop v*) pbody))

                (`(reset ,body) (ast-reset (loop body)))
                (`(shift ,k ,body)
                  (define k-raw-addr (fresh-name 'k-raw))
                  (define k-addr     (fresh-name 'k))
                  (define arg-addr   (fresh-name 'arg))
                  (define inner-body
                    ($let (list k) (list k-addr)
                          (list ($lambda
                                  #f (list arg-addr) (list arg-addr)
                                  (lambda _
                                    (ast-unshift (ast-variable k-raw-addr)
                                                 (ast-variable arg-addr)))))
                          ($b*->body env body)))
                  (ast-shift ($lambda #f (list k-raw-addr) (list k-raw-addr)
                                      (lambda _ inner-body))))

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

  ;; let*, letrec, letrec*
  ;; quasiquote

  ;; cond, and, or, when, unless, case, match

  ;; let-syntax, letrec-syntax
  )

(define (expand/env env form)
  (define expanded (expand-once env form))
  (if (ast? expanded) expanded (expand/env env expanded)))

(define (expand form) (expand/env env-empty form))


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
