#lang racket/base
(provide
  term
  term?
  term-source
  term-datum
  exception
  exception?
  exception-description
  exception-datum
  syntax-transformer
  expand
  env-empty
  env-alias
  env-extend*
  variable-binding*
  keyword-binding*
  env-initial
  evaluate
  )

(require
  "mk.rkt"
  "syntax.rkt"
  "type.rkt"
  racket/vector
  )

;; TODO:
;; syntax-error for more convenient source-info reporting.
;; match-syntax for more convenient parsing and validation.

(define-variant-type
  b-entry?
  (b-keyword b-keyword? b-keyword-transformer)
  (b-variable b-variable? b-variable-address))

(define env-empty (hash))
(define (env-set env i value) (hash-set env (identifier->label i) value))
(define (env-ref/default env i default)
  (hash-ref env (identifier->label i) default))
(define (env-ref env i) (env-ref/default env i #f))
(define (env-alias env alias aliased)
  (define value (env-ref env aliased))
  (when (not value) (error "cannot alias unbound identifier:" aliased alias))
  (env-set env alias value))
(define (env-extend* env b*)
  (foldl (lambda (b env) (env-set env (car b) (cdr b))) env b*))

(define (variable-binding* i*) (map (lambda (i) (cons i (b-variable i))) i*))
(define (keyword-binding* b*)
  (map (lambda (b)
         (define name (car b))
         (define i (if (identifier? name) name (datum->syntax #f name)))
         (cons i (b-keyword (cdr b)))) b*))

(define (strip-syntax d) (syntax->datum (datum->syntax #f d)))
(define-type
  (exception exception (lambda (e) (list (exception-description e)
                                         (strip-syntax (exception-datum e)))))
  exception? exception-description exception-datum)
(define-type (term term (lambda (d) (list (strip-syntax (term-datum d)))))
  term? (term-source term-source-set) (term-datum term-datum-set))
(define (term-source-prepend tm form)
  (term-source-set tm (cons form (term-source tm))))

(define (form->transformer env form)
  (and (identifier? form)
       (let ((b (env-ref env form)))
         (and (b-keyword? b) (b-keyword-transformer b)))))

(define (datum-self-evaluating? datum)
  (or (boolean? datum) (number? datum) (char? datum) (string? datum)))
(define (datum-atom? datum)
  (or (datum-self-evaluating? datum) (symbol? datum) (null? datum)))
(define (datum-valid-literal? datum)
  (cond ((vector? datum) (andmap datum-valid-literal? (vector->list datum)))
        ((pair? datum) (and (datum-valid-literal? (car datum))
                            (datum-valid-literal? (cdr datum))))
        (else (datum-atom? datum))))

(define (syntax-self-evaluating? stx)
  (datum-self-evaluating? (syntax->datum stx)))
(define (syntax->~list stx)
  (define d (syntax-unwrap stx))
  (cond ((null? d) '())
        ((pair? d) (cons (car d) (syntax->~list (cdr d))))
        (else (list stx))))

(define (bound-identifier-unique? i i*)
  (or (null? i*) (and (or (not (identifier? (car i*)))
                          (not (bound-identifier=? i (car i*))))
                      (bound-identifier-unique? i (cdr i*)))))
(define (bound-identifier-unique* i*)
  (if (null? i*) '()
    (let ((i (car i*)))
      (cons (if (and (identifier? i)
                     (not (bound-identifier-unique? i (cdr i*))))
              (exception 'duplicate-parameter i)
              i)
            (bound-identifier-unique* (cdr i*))))))
(define (param-identifier* p*)
  (if (null? p*) '()
    (let ((p (car p*)))
      (cons (if (or (not (syntax-unwrap p)) (identifier? p)) p
              (exception 'invalid-parameter p))
            (param-identifier* (cdr p*))))))
(define (formal-param* s*)
  (reverse (bound-identifier-unique* (param-identifier* (reverse s*)))))
(define (variadic-formal-param*? stx)
  (define d (syntax-unwrap stx)) (null? d)
  (and (not (null? d))
       (or (not (pair? d)) (variadic-formal-param*? (cdr d)))))
(define (renamed-formal-param* p*)
  (if (null? p*) '()
    (let ((p (car p*)))
      (cons (if (identifier? p) (identifier-rename p) p)
            (renamed-formal-param* (cdr p*))))))

(define (term-exception? t) (exception? (term-datum t)))
(define (build-literal form)
  (if (not (datum-valid-literal? (syntax->datum form)))
    (exception 'quote form)
    `#(quote ,form)))
(define (build-variable identifier address) `#(var ,identifier ,address))
(define (build-apply proc arg*)
  (define tm `#(apply ,proc ,(list->vector arg*)))
  (if (or (term-exception? proc) (ormap term-exception? arg*))
    (exception #f tm)
    tm))

(define (build-if c t f)
  (define tm `#(if ,c ,t ,f))
  (if (ormap term-exception? (list c t f)) (exception #f tm) tm))

(define (build-lambda env variadic? p* i*->body)
  (define param* (formal-param* p*))
  (define i?* (renamed-formal-param* param*))
  (define i* (filter identifier? i?*))
  (define body (i*->body i*))
  (define tm `#(lambda ,variadic? ,(list->vector i?*) ,body))
  (if (term-exception? body) (exception #f tm) tm))

(define (expand/k k env form)
  (define dform (syntax-unwrap form))
  (define expanded
    (cond ((syntax-self-evaluating? form) (build-literal form))
          ((identifier? form)
           (define b (env-ref env form))
           (cond ((b-variable? b) (build-variable form (b-variable-address b)))
                 ((b-keyword? b) ((b-keyword-transformer b) k env form))
                 (else (exception 'unbound-variable form))))
          ((pair? dform)
           (define head (car dform))
           (define transformer (form->transformer env head))
           (if transformer (transformer k env form) (expand-apply env form)))
          (else (exception 'unknown form))))
  (if (term? expanded) (term-source-prepend expanded form)
    (term (list form) expanded)))
(define (expand env form) (expand/k expand env form))
(define (syntax-transformer proc)
  (lambda (k env stx)
    (define result ((procedure->hygienic-syntax-transformer proc) stx))
    (if (exception? result) result (k env result))))

(define-syntax match/mk
  (syntax-rules ()
    ((_) (error "match/mk all clauses failed"))
    ((_ (((qvs ...) g ...) body ...) c* ...)
     (let () (begin (define result* (run* (qvs ...) g ...))
                    (if (not (pair? result*)) (match/mk c* ...)
                      (apply (lambda (qvs ...) body ...) (car result*))))))))

(define (expand-top env original-form rest*)
  (let loop ((env env) (form original-form))
    (match/mk
      (((f*) (== #`(begin . #,f*) form)) (expand-top* env f* rest*))

      (((name p* body+) (== #`(define (#,name . #,p*) . #,body+) form))
       (expand-top env #`(define #,name (lambda #,p* . #,body+)) rest*))

      (((name body) (== #`(define #,name #,body) form))
       (define i* (filter identifier? (renamed-formal-param* (list name))))
       (define env-rest (env-extend* env (variable-binding* i*)))
       (define (rename f) (syntax-rename/identifier* f i*))
       (define renamed-rest* (map rename rest*))
       (cons (cons name body) (expand-top* env-rest #'() renamed-rest*)))

      ((() succeed)
       (define expanded-form
         (let ((dform (syntax-unwrap form)))
           (cond ((identifier? form)
                  (define b (env-ref env form))
                  (and (b-keyword? b)
                       ((b-keyword-transformer b) loop env form)))
                 ((pair? dform)
                  (define head (car dform))
                  (define transformer (form->transformer env head))
                  (and transformer (transformer loop env form)))
                 (else #f))))
       (if (syntax? expanded-form)
         (loop env expanded-form)
         (cons (cons #f original-form) (expand-top* env #'() rest*)))))))

(define (expand-top* env form* rest*)
  (match/mk
    ((() (== #'() form*) (== '() rest*)) '())

    (((f* f**) (== #'() form*) (== `(,f* . ,f**) rest*))
     (expand-top* env f* f**))

    (((a d) (== #`(#,a . #,d) form*)) (expand-top env a (cons d rest*)))

    ((() succeed)
     (cons (exception 'top-nonlist form*) (expand-top* env #'() rest*)))))

(define (i*->expanded-body old-env body+)
  (lambda (i*)
    (define renamed-form (syntax-rename/identifier* body+ i*))
    (define env (env-extend* old-env (variable-binding* i*)))
    (define definitions (expand-top* env renamed-form '()))
    (cond ((null? definitions) (exception 'body-empty body+))
          (else (define rdefs (reverse definitions))
                (define b* (map (lambda (ba) (list (car ba) (cdr ba)))
                                (reverse (cdr rdefs))))
                (define body-b (car rdefs))
                (define body (cdr body-b))
                (cond ((car body-b) (exception 'body-no-expressions body+))
                      ((null? b*) (expand env body))
                      ;; TODO: use letrec* instead.
                      (else (expand env #`(let* #,b* #,body))))))))

(define (expand-lambda _ env form)
  (match/mk
    (((p* body+ _) (== #`(#,_ #,p* . #,body+) form))
     (define variadic? (variadic-formal-param*? p*))
     (define param* (syntax->~list p*))
     (build-lambda env variadic? param* (i*->expanded-body env body+)))
    ((() succeed) (exception 'lambda form))))

(define (expand-apply env form)
  (define dform (syntax-unwrap form))
  (define rargs
    (let loop ((fa* (cdr dform)) (a* '()))
      (match/mk
        ((() (== #'() fa*)) a*)
        (((a d) (== #`(#,a . #,d) fa*)) (loop d (cons (expand env a) a*)))
        ((() succeed) (exception 'apply-args fa*)))))
  (if (exception? rargs) (exception 'apply form)
    (build-apply (expand env (car dform)) (reverse rargs))))

(define (expand-quote _ env form)
  (match/mk (((literal _) (== #`(#,_ #,literal) form)) (build-literal literal))
            ((() succeed) (exception 'quote form))))

(define (expand-if _ env form)
  (match/mk
    (((c t f _) (== #`(#,_ #,c #,t #,f) form))
     (build-if (expand env c) (expand env t) (expand env f)))
    ((() succeed) (exception 'if form))))

(define expand-let
  (syntax-transformer
    (lambda (form)
      (match/mk
        ;; TODO: symbolo
        ;(((_ name b* body) (== #`(#,_ #,name #,b* #,body) form) (symbolo name))
         ;)
        (((_ b* body) (== #`(#,_ #,b* . #,body) form))
         (let loop ((b* b*) (p* '()) (a* '()))
           (match/mk
             (((p a b*-rest) (== #`((#,p #,a) . #,b*-rest) b*))
              (loop b*-rest (cons p p*) (cons a a*)))
             ((() (== #'() b*))
              #`((lambda #,(reverse p*) . #,body) . #,(reverse a*)))
             ((() succeed) (exception 'let form)))))
        ((() succeed) (exception 'let form))))))

(define expand-let*
  (syntax-transformer
    (lambda (form)
      (match/mk
        (((_ body) (== #`(#,_ () . #,body) form))
         #`(let () . #,body))

        (((_ p a b* body) (== #`(#,_ ((#,p #,a) . #,b*) . #,body) form))
         #`(let ((#,p #,a)) (let* #,b* . #,body)))

        ((() succeed) (exception 'let* form))))))

(define env-initial
  (env-extend*
    env-empty
    (keyword-binding*
      `((quote . ,expand-quote)
        (lambda . ,expand-lambda)
        (if . ,expand-if)
        ;(letrec . ,expand-letrec)
        ;(letrec* . ,expand-letrec)
        (let . ,expand-let)
        (let* . ,expand-let*)
        ;; TODO: (Some of these expanders can be implemented as transformers.)
        ;(quasiquote . ,expand-quasiquote)
        ;(syntax . ,expand-syntax)
        ;(quasisyntax . ,expand-quasisyntax)
        ;(begin . ,expand-begin)
        ;(cond . ,expand-cond)
        ;(case . ,expand-case)
        ;(match . ,expand-match)
        ;(and . ,expand-and)
        ;(or . ,expand-or)
        ;(when . ,expand-when)
        ;(unless . ,expand-unless)
        ;(set! . ,expand-set!)
        ;(reset . ,expand-reset)
        ;(shift . ,expand-shift)
        ;(let-syntax . ,expand-let-syntax)
        ;(letrec-syntax . ,expand-letrec-syntax)
        ;(letrec*-syntax+values . ,expand-letrec*-syntax+values)
        ;splicing variants...
        ))))


(define-type closure closure?
  closure-variadic? closure-param* closure-body closure-env)

(define (evaluate depth env tm)
  (define e (term-datum tm))
  (if (and depth (<= depth 0)) (exception 'out-of-depth `(,tm ,env))
    (match/mk
      (((id address) (== `#(var ,id ,address) e))
       (env-ref/default env address (exception 'unbound-variable tm)))

      (((dform) (== `#(quote ,dform) e)) (syntax->datum dform))

      (((variadic? p* body) (== `#(lambda ,variadic? ,p* ,body) e))
       (closure variadic? p* body env))

      (((tc tt tf) (== `#(if ,tc ,tt ,tf) e))
       (define c (evaluate depth env tc))
       (cond ((exception? c)
              (exception #f (term-datum-set tm `#(if ,c ,tt ,tf))))
             (c (evaluate depth env tt))
             (else (evaluate depth env tf))))

      (((tproc targ*) (== `#(apply ,tproc ,targ*) e))
       (define proc (evaluate depth env tproc))
       (define arg* (vector-map (lambda (ta) (evaluate depth env ta)) targ*))
       (define (evaluate-apply a*)
         (define b?*
           (vector-map (lambda (p a) (and (identifier? p) (cons p a)))
                       (closure-param* proc) a*))
         (define b* (vector-filter (lambda (x) x) b?*))
         (define env^
           (foldl (lambda (b env) (env-set env (car b) (cdr b)))
                  (closure-env proc) (vector->list b*)))
         (evaluate (and depth (- depth 1)) env^ (closure-body proc)))

       (cond ((not (closure? proc))
              (exception 'inapplicable-procedure
                         (term-datum-set tm `#(apply ,proc ,arg*))))
             ((ormap exception? (vector->list arg*))
              (exception #f (term-datum-set tm `#(apply ,proc ,arg*))))
             (else
               (define p* (closure-param* proc))
               (cond ((and (closure-variadic? proc)
                           (<= (- (vector-length p*) 1) (vector-length arg*)))
                      (define a0* (vector-take arg* (- (vector-length p*) 1)))
                      (define a1* (vector-drop arg* (- (vector-length p*) 1)))
                      (evaluate-apply
                        (vector-append a0* (vector (vector->list a1*)))))
                     ((= (vector-length p*) (vector-length arg*))
                      (evaluate-apply arg*))
                     (else (exception 'argument-count-mismatch
                                      (term-datum-set
                                        tm `#(apply ,proc ,arg*))))))))

      ((() succeed)
       (cond ((and (exception? e) (not (exception-description e)))
              (evaluate depth env (term-datum-set tm (exception-datum e))))
             ((exception? e) e)
             (else (exception 'unhandled-term tm)))))))
