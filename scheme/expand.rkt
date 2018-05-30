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
  expand-once
  env-empty
  env-alias
  env-extend*
  variable-binding-value*
  variable-binding*
  keyword-binding*
  env-initial
  evaluate
  stdlib
  )

(require
  "match.rkt"
  "syntax.rkt"
  "type.rkt"
  racket/list
  racket/vector
  )

(define-type*
  b-entry?
  (b-keyword b-keyword? b-keyword-transformer)
  (b-variable b-variable? b-variable-address))

(define env-empty (hash))
(define (env-set env i value) (hash-set env (identifier->free i) value))
(define (env-ref/default env i default)
  (hash-ref env (identifier->free i) default))
(define (env-ref env i) (env-ref/default env i #f))
(define (env-alias env alias aliased)
  (define value (env-ref env aliased))
  (when (not value) (error "cannot alias unbound identifier:" aliased alias))
  (env-set env alias value))
(define (env-extend* env b*)
  (foldl (lambda (b env) (env-set env (car b) (cdr b))) env b*))

(define (env-rib p a) (cons p (box a)))
(define (variable-binding-value* b*)
  (map (lambda (b)
         (define name (car b))
         (define i (if (identifier? name) name (datum->syntax #f name)))
         (env-rib i (cdr b))) b*))
(define (variable-binding* i*) (map (lambda (i) (cons i (b-variable i))) i*))
(define (keyword-binding* b*)
  (map (lambda (b)
         (define name (car b))
         (define i (if (identifier? name) name (datum->syntax #f name)))
         (cons i (b-keyword (cdr b)))) b*))

(define (strip-syntax d) (syntax->datum (datum->syntax #f d)))
(define-type
  (exception _exception (lambda (e) (list (exception-description e)
                                          (strip-syntax (exception-datum e)))))
  exception? exception-description exception-datum)
(define-type (term term (lambda (d) (list (strip-syntax (term-datum d)))))
  term? (term-source term-source-set) (term-datum term-datum-set))
(define (term-source-prepend tm form)
  (term-source-set tm (cons form (term-source tm))))
(define (raise-exception e)
  (define datum (exception-datum e))
  (define details
    (let loop ((datum datum))
      (cond ((syntax? datum) `(,(strip-syntax datum) ,(syntax-metadata datum)))
            ((term? datum)
             `(term: ,(term-datum datum) . ,(map loop (term-source datum))))
            (else (list datum)))))
  (apply error "exception:" (exception-description e) details))
;; Swap commenting to choose between best-effort and immediate failure.
;(define (exception . details) (apply _exception details))
(define (exception . details) (raise-exception (apply _exception details)))

(define (form->transformer env form)
  (define dform (syntax-unwrap form))
  (define i (if (pair? dform) (car dform) form))
  (and (identifier? i)
       (let ((b (env-ref env i)))
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
(define (param? p)
  (and (syntax? p) (or (not (syntax-unwrap p)) (identifier? p))))
(define (param-identifier* p*)
  (if (null? p*) '()
    (let ((p (car p*)))
      (cons (if (param? p) p (exception 'invalid-parameter p))
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
(define build-undefined '#(undefined))
(define (build-literal form)
  (if (not (datum-valid-literal? (syntax->datum form)))
    (exception 'quote form)
    `#(quote ,form)))
(define (build-variable identifier address) `#(var ,identifier ,address))
(define (build-set! var value) `#(set! ,var ,value))
(define (build-apply proc arg*)
  (define tm `#(apply ,proc ,(list->vector arg*)))
  (if (or (term-exception? proc) (ormap term-exception? arg*))
    (exception #f tm)
    tm))
(define (build-apply-list proc args)
  (define tm `#(apply-list ,proc ,args))
  (if (or (term-exception? proc) (term-exception? args))
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

(define (build-primitive-op name a*)
  `#(primitive-op ,name ,(list->vector a*)))

(define (expand-once env form)
  (cond ((form->transformer env form) => (lambda (t) (t env form)))
        ((syntax-self-evaluating? form) (build-literal form))
        ((identifier? form)
         (define b (env-ref env form))
         (cond ((b-variable? b) (build-variable form (b-variable-address b)))
               (else (exception 'unbound-variable form))))
        ((pair? (syntax-unwrap form)) (expand-apply env form))
        (else (exception 'unknown form))))
(define (expand env form)
  (let loop ((expanded (expand-once env form)))
    (cond ((syntax? expanded) (loop (expand-once env expanded)))
          ((term? expanded) (term-source-prepend expanded form))
          (else (term (list form) expanded)))))
(define (syntax-transformer proc)
  (define transform (procedure->hygienic-syntax-transformer proc))
  (lambda (env stx) (transform stx)))
(define (generate-identifier i)
  (identifier-rename
    ((procedure->hygienic-syntax-transformer (lambda (form) i)) i)))

(define (expand-top env original-form rest*)
  (let loop ((env env) (form original-form))
    (match form
      (#`(begin . #,(and f* (list _ ...))) (expand-top* env f* rest*))

      (#`(define (#,(? identifier? name) . #,p*) . #,(list body ..1))
       (expand-top env #`(define #,name (lambda #,p* . #,body)) rest*))

      (#`(define #,(? identifier? name) #,body)
       (define i* (filter identifier? (renamed-formal-param* (list name))))
       (define env-rest (env-extend* env (variable-binding* i*)))
       (define (rename f) (syntax-rename/identifier* f i*))
       (define renamed-rest* (map rename rest*))
       (cons (cons name body) (expand-top* env-rest #'() renamed-rest*)))

      (#`(begin . #,_) (cons (cons #f (exception 'top-begin form))
                             (expand-top* env #'() rest*)))
      (#`(define . #,_) (cons (cons #f (exception 'top-define form))
                              (expand-top* env #'() rest*)))

      (_ (define expanded-form (expand-once env form))
         (if (syntax? expanded-form)
           (loop env expanded-form)
           (cons (cons #f original-form) (expand-top* env #'() rest*)))))))

(define (expand-top* env form* rest*)
  (match (cons form* rest*)
    ((cons #'() '()) '())
    ((cons #'() `(,f* . ,f**)) (expand-top* env f* f**))
    ((cons #`(#,a . #,d) _) (expand-top env a (cons d rest*)))
    (_ (cons (cons #f (exception 'top-nonlist form*))
             (expand-top* env #'() rest*)))))

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
                      ((null? (filter (lambda (x) x) (map car b*)))
                       (expand env #`(let* #,b* #,body)))
                      (else (expand env #`(letrec* #,b* #,body))))))))

(define (expand-lambda env form)
  (match form
    (#`(#,_ #,p* . #,(and body (list _ ..1)))
     (define variadic? (variadic-formal-param*? p*))
     (define param* (syntax->~list p*))
     (build-lambda env variadic? param* (i*->expanded-body env body)))
    (_ (exception 'lambda form))))

(define (expand-apply env form)
  (define dform (syntax-unwrap form))
  (define proc (expand env (car dform)))
  (define args (match (cdr dform)
                 (#`(#,@a*) (map (lambda (a) (expand env a)) a*))
                 (_ (exception 'apply-args (cdr dform)))))
  (if (exception? args) (exception 'apply form) (build-apply proc args)))

(define (expand-apply-list env form)
  (match form
    (#`(#,_ #,proc #,args)
     (build-apply-list (expand env proc) (expand env args)))
    (_ (exception 'apply-list form))))

(define-type undefined undefined?)
(define i-undefined (generate-identifier #'undefined))

(define (expand-undefined env form)
  (match form
    ((? identifier?) build-undefined)
    (_ (exception 'malformed-undefined form))))

(define (expand-quote env form)
  (match form
    (#`(#,_ #,literal) (build-literal literal))
    (_ (exception 'quote form))))

(define (expand-if env form)
  (match form
    (#`(#,_ #,c #,t #,f)
     (build-if (expand env c) (expand env t) (expand env f)))
    (_ (exception 'if form))))

(define expand-ignore
  (syntax-transformer (lambda (form) #`(let ((#f #,form)) #t))))

(define (expand-set! env form)
  (match form
    (#`(#,_ #,(? identifier? name) #,value)
     (define b (env-ref env name))
     (cond ((b-variable? b)
            (build-set! (build-variable form (b-variable-address b))
                        (expand env value)))
           ((b-keyword? b) (exception 'cannot-set!-keyword name))
           (else (exception 'unbound-variable name))))
    (#`(#,_ #f #,value) (expand-ignore env value))
    (_ (exception 'set! form))))

(define expand-letrec*
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ #,(list `(,p ,a) ...) . #,(list body ..1))
         #`(let #,(map (lambda (x) #`(#,x #,i-undefined)) p)
             #,@(map (lambda (x e) #`(set! #,x #,e)) p a)
             . #,body))
        (_ (exception 'letrec* form))))))

;; NOTE: letrec is harder to implement than letrec*, and is less expressive.
(define expand-letrec
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ #,(list `(,p ,a) ...) . #,(list body ..1))
         (define t* (map (lambda (x) (generate-identifier x)) p))
         #`(let #,(map (lambda (x) #`(#,x #,i-undefined)) p)
             (let #,(map (lambda (t e) #`(#,t #,e)) t* a)
               #,@(map (lambda (x t) #`(set! #,x #,t)) p t*)
               . #,body)))
        (_ (exception 'letrec* form))))))

(define expand-let
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ #,(list `(,p ,a) ...) . #,(list body ..1))
         #`((lambda #,p . #,body) . #,a))
        (_ (exception 'let form))))))

(define expand-let*
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ () . #,(list body ..1)) #`(let () . #,body))

        (#`(#,_ ((#,p #,a) . #,b*) . #,(list body ..1))
         #`(let ((#,p #,a)) (let* #,b* . #,body)))

        (_ (exception 'let* form))))))

(define expand-begin
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ . #,(list body ..1)) #`(let () . #,body))
        (_ (exception 'begin form))))))

(define expand-and
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_) #'#t)

        (#`(#,_ #,single) single)

        (#`(#,_ #,first . #,(list rest ...)) #`(if #,first (and . #,rest) #f))

        (_ (exception 'and form))))))

(define expand-or
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_) #'#f)

        (#`(#,_ #,single) single)

        (#`(#,_ #,first . #,(list rest ...))
         #`(let ((fst #,first)) (if fst fst (or . #,rest))))

        (_ (exception 'or form))))))

(define expand-when
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ #,c . #,t) #`(if #,c (let () . #,t) #t))
        (_ (exception 'when form))))))

(define expand-unless
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(#,_ #,c . #,f) #`(if #,c #t (let () . #,f)))
        (_ (exception 'unless form))))))

(define expand-cond
  (syntax-transformer
    (lambda (form)
      (match form
        (#`(cond) #`('error:cond:no-matching-clause))
        (#`(cond (else . #,body)) #`(let () . #,body))
        (#`(cond (#,c => #,p) . #,cs)
         #`(let ((v #,c)) (if v (#,p v) (cond . #,cs))))
        (#`(cond (#,c . #,body) . #,cs)
         #`(if #,c (let () . #,body) (cond . #,cs)))
        (_ (exception 'cond form))))))

(define expand-quasiquote
  (syntax-transformer
    (lambda (form)
      (define (bad msg) (exception `(,'quasiquote ,msg) form))
      (define (build-pair a d) #`(cons #,a #,d))
      (define (build-l->v xs) #`(list->vector #,xs))
      (define (build-append xs ys) #`(append #,xs #,ys))
      (define (tag t e) (build-pair #`(quote #,t) (build-pair e #'(quote ()))))
      (match form
        (#`(#,_ #,qqf)
         (let loop ((level 0) (qqf qqf))
           (match qqf
             (#`(quasiquote #,qq) (tag 'quasiquote (loop (+ level 1) qq)))
             (#`(unquote #,uq)
              (if (= 0 level) uq (tag 'unquote (loop (- level 1) uq))))
             (#`((unquote-splicing #,uqs) . #,qq)
              (define qqd (loop level qq))
              (if (= 0 level) (build-append uqs qqd)
                #`(#,(tag 'unquote-splicing (loop (- level 1) uqs)) . #,qqd)))

             (#`(quasiquote . #,x)       (bad 'quasiquote))
             (#'unquote                  (bad 'unquote))
             (#`(unquote . #,_)          (bad 'unquote))
             (#'unquote-splicing         (bad 'unquote-splicing))
             (#`(unquote-splicing . #,_) (bad 'unquote-splicing))

             (#`#(#,@vqq) (build-l->v (loop level (datum->syntax #f vqq))))
             (#`(#,qqa . #,qqd) (build-pair (loop level qqa) (loop level qqd)))
             (literal            #`(quote #,literal)))))
        (_ (bad 'quasiquote))))))

(define (expand-primitive-op name arity)
  (lambda (env form)
    (match form
      (#`(#,_ . #,(list a* ...))
       (if (= arity (length a*))
         (build-primitive-op name (map (lambda (a) (expand env a)) a*))
         (exception `(bad primitive op: ,name ,arity) form)))
      (_ (exception `(bad primitive op: ,name ,arity) form)))))


(define-type closure closure?
  closure-variadic? closure-param* closure-body closure-env)

(define-type mvector mvector? mvector-v)

(define (make-mvector i k) (mvector (make-vector i k)))
(define (mvector-set! mv i v) (vector-set! (mvector-v mv) i v))
(define (mvector->vector mv) (vector-copy (mvector-v mv)))

(define primitive-ops
  `((eqv? (,datum-atom? ,datum-atom?) ,eqv?)

    (procedure?      (#f) ,closure?)
    (mutable-vector? (#f) ,mvector?)
    (vector?  (#f) ,vector?)
    (pair?    (#f) ,pair?)
    (null?    (#f) ,null?)
    (string?  (#f) ,string?)
    (char?    (#f) ,char?)
    (number?  (#f) ,number?)
    (integer? (#f) ,integer?)
    (symbol?  (#f) ,symbol?)
    (boolean? (#f) ,boolean?)
    (not      (#f) ,not)

    (char->integer  (,char?)    ,char->integer)
    (integer->char  (,integer?) ,integer->char)
    (string->vector (,string?)  ,(lambda (s) (list->vector (string->list s))))
    (vector->string (,vector?)  ,(lambda (v) (list->string (vector->list v))))
    (string->symbol (,string?)  ,string->symbol)
    (symbol->string (,symbol?)  ,symbol->string)

    (cons (#f #f)  ,cons)
    (car  (,pair?) ,car)
    (cdr  (,pair?) ,cdr)

    (vector-ref    (,vector? ,integer?) ,vector-ref)
    (vector-length (,vector?)           ,vector-length)

    (make-mutable-vector    (,integer? #f)           ,make-mvector)
    (mutable-vector-set!    (,mvector? ,integer? #f) ,mvector-set!)
    (mutable-vector->vector (,mvector?)              ,mvector->vector)

    (=  (,number? ,number?) ,=)
    (<= (,number? ,number?) ,<=)
    (<  (,number? ,number?) ,<)
    (+  (,number? ,number?) ,+)
    (*  (,number? ,number?) ,*)

    ;bitwise-and
    ;bitwise-ior
    ;bitwise-xor
    ;bitwise-not
    ;bitwise-bit-set?
    ;bitwise-bit-field
    ;arithmetic-shift
    ;integer-length

    ;round
    ;quotient
    ;remainder

    ;; These could be derived instead.
    (-  (,number? ,number?) ,-)
    (/  (,number? ,number?) ,/)

    (append (,list? #f) ,append)
    (list->vector (,list?) ,list->vector)
    ))

;(define code-append
  ;#'(letrec ((append
               ;(lambda (xs ys)
                 ;(if (null? xs)
                   ;ys
                   ;(cons (car xs) (append (cdr xs) ys))))))
      ;append))

;(define code-list->vector
  ;#'(letrec ((length (lambda (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))
             ;(loop (lambda (mv i xs)
                     ;(unless (null? xs)
                       ;(mutable-vector-set! mv i (car xs))
                       ;(loop mv (+ 1 i) (cdr xs)))))
             ;(list->vector
               ;(lambda (xs)
                 ;(define size (length xs))
                 ;(define mv (make-mutable-vector size #t))
                 ;(loop mv 0 xs)
                 ;(mutable-vector->vector mv))))
      ;list->vector))

;'apply' should not be a normal op

(define derived-ops
  #`((mutable-vector-length
       (lambda (mv) (vector-length (mutable-vector->vector mv))))
     (mutable-vector-ref
       (lambda (mv i) (vector-ref (mutable-vector->vector mv) i)))

     (vector (lambda xs (list->vector xs)))
     ;vector-set
     ;vector-append
     ;vector-reverse
     ;vector-map
     ;vector-filter
     ;vector-foldl
     ;vector-foldr

     (list? (lambda (v) (or (and (pair? v) (list? (cdr v))) (null? v))))
     (list (lambda xs xs))

     (cons* (lambda (x xs) (if (pair? xs)
                             (cons x (cons* (car xs) (cdr xs)))
                             x)))
     (list* (lambda (x . xs) (cons* x xs)))

     ;list-ref
     ;list-tail
     ;list-set

     ;(list->vector #,code-list->vector)
     ;vector->list
     ;list->string
     ;string->list

     ;length
     ;(foldl (lambda (f acc xs)
              ;(if (null? xs) acc (foldl f (f (car xs) acc) (cdr xs)))))
     ;(foldr (lambda (f acc xs)
              ;(if (null? xs) acc (f (car xs) (foldr f acc (cdr xs))))))
     ;(append #,code-append)
     ;reverse-append
     ;reverse
     ;map
     ;filter

     ;assoc
     ;member

     (apply (lambda (f x . xs) (apply f (cons* x xs))))
     ))

(define (stdlib program)
  (define (po->def po)
    (define name (car po))
    (define p*
      (map (lambda (i) (string->symbol (string-append "x" (number->string i))))
           (range (length (cadr po)))))
    `(,name (lambda ,p* (,name . ,p*))))

  #`(let #,(map po->def primitive-ops)
      (letrec #,derived-ops #,program)))

(define primitive-op-expanders
  (keyword-binding*
    (map (lambda (po-desc)
           (define name (car po-desc))
           (define arity (length (cadr po-desc)))
           (cons name (expand-primitive-op name arity)))
         primitive-ops)))

(define env-initial
  (env-extend*
    env-empty
    (append
      (keyword-binding*
        `((,i-undefined . ,expand-undefined)
          (apply . ,expand-apply-list)
          (quote . ,expand-quote)
          (,'quasiquote . ,expand-quasiquote)
          (lambda . ,expand-lambda)
          (if . ,expand-if)
          (set! . ,expand-set!)
          (letrec . ,expand-letrec)
          (letrec* . ,expand-letrec*)
          (let . ,expand-let)
          (let* . ,expand-let*)
          (begin . ,expand-begin)
          ;; TODO: (Some of these expanders can be implemented as transformers.)
          ;(syntax . ,expand-syntax)
          ;(quasisyntax . ,expand-quasisyntax)
          (cond . ,expand-cond)
          ;(case . ,expand-case)
          ;(match . ,expand-match)
          (and . ,expand-and)
          (or . ,expand-or)
          (when . ,expand-when)
          (unless . ,expand-unless)
          ;(reset . ,expand-reset)
          ;(shift . ,expand-shift)
          ;(let-syntax . ,expand-let-syntax)
          ;(letrec-syntax . ,expand-letrec-syntax)
          ;(letrec*-syntax+values . ,expand-letrec*-syntax+values)
          ;splicing variants...
          ))
      primitive-op-expanders)))

(define primitive-op-evaluators
  (make-immutable-hash
    (map (lambda (po-desc)
           (define name (car po-desc))
           (define arg-sig (cadr po-desc))
           (define op (caddr po-desc))
           (define (validate a*)
             (andmap (lambda (ty? a) (or (not ty?) (ty? a))) arg-sig a*))
           (define (full-op blame a*)
             (if (validate a*)
               (apply op a*)
               (exception `(primitive-op-type-error ,arg-sig ,a*) blame)))
           (cons name full-op)) primitive-ops)))


(define (evaluate depth env tm)
  (define (evaluate-apply proc arg*)
    (define (continue a*)
      (define b?*
        (vector-map (lambda (p a) (and (identifier? p) (env-rib p a)))
                    (closure-param* proc) a*))
      (define b* (vector-filter (lambda (x) x) b?*))
      (define env^ (env-extend* (closure-env proc) (vector->list b*)))
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
                   (continue (vector-append a0* (vector (vector->list a1*)))))
                  ((= (vector-length p*) (vector-length arg*)) (continue arg*))
                  (else (exception 'argument-count-mismatch
                                   (term-datum-set
                                     tm `#(apply ,proc ,arg*))))))))
  (define e (term-datum tm))
  (if (and depth (<= depth 0)) (exception 'out-of-depth `(,tm ,env))
    (match e
      (`#(var ,id ,address)
        (define (exc) (box (exception 'unbound-variable tm)))
        (define v (unbox (env-ref/default env address exc)))
        (if (undefined? v) (exception 'uninitialized-variable tm) v))

      (`#(quote ,dform) (syntax->datum dform))

      (`#(lambda ,variadic? ,p* ,body) (closure variadic? p* body env))

      (`#(if ,tc ,tt ,tf)
        (define c (evaluate depth env tc))
        (cond ((exception? c)
               (exception #f (term-datum-set tm `#(if ,c ,tt ,tf))))
              (c (evaluate depth env tt))
              (else (evaluate depth env tf))))

      (`#(apply ,tproc ,targ*)
        (define proc (evaluate depth env tproc))
        (define arg* (vector-map (lambda (ta) (evaluate depth env ta)) targ*))
        (evaluate-apply proc arg*))

      (`#(apply-list ,tproc ,targlist)
        (define proc (evaluate depth env tproc))
        (define arglist (evaluate depth env targlist))
        (if (list? arglist)
          (evaluate-apply proc (list->vector arglist))
          (exception 'apply-list-non-list `#(apply-list ,proc ,arglist))))

      (`#(primitive-op ,name ,targ*)
        (define op (hash-ref primitive-op-evaluators name))
        (define (ev ta) (evaluate depth env ta))
        (define a* (vector->list (vector-map ev targ*)))
        (op tm a*))

      (`#(set! #(var ,id ,address) ,tvalue)
        (define (exc) (exception 'unbound-variable tm))
        (define v (env-ref/default env address exc))
        (cond ((box? v) (set-box! v (evaluate depth env tvalue)) #t)
              ((exception? v) v)
              (else (exception `(set!-failed: ,v) tm))))

      (`#(undefined) undefined)

      (_ (cond ((and (exception? e) (not (exception-description e)))
                (evaluate depth env (term-datum-set tm (exception-datum e))))
               ((exception? e) e)
               (else (exception 'unhandled-term tm)))))))
