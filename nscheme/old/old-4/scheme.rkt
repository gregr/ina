#lang racket/base
(provide
  scheme-eval
  )

(require
  "ast.rkt"
  "data.rkt"
  "eval-ast.rkt"
  "match.rkt"
  "syntax.rkt"
  "type.rkt"
  racket/control
  racket/list
  (only-in racket/set set-count list->set)
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

(define-vector-type expander expander? expander-proc)

(define (literal? form)
  (or (boolean? form) (number? form) (char? form) (string? form)))

(define (expand env form)
  (define (loop d) (expand env d))
  (cond ((form->transformer env form) => (lambda (t) (loop (t env form))))
        ((form->parser env form)      => (lambda (p)       (p env form)))
        ((expander? form) ((expander-proc form) env))
        ((literal? form) (ast-literal form))
        ((closed-name? form)
         (expand (closed-name-env form) (closed-name-n form)))
        ((name? form) (ast-variable (env-ref-lexical env form)))
        (else (match-syntax env form
                (`(,p ,@a*) (ast-apply* (loop p) (map loop a*)))
                (_          (error "invalid syntax:" form))))))

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

;;; Syntax utilities
(define (reverse-append xs ys)
  (if (null? xs) ys (reverse-append (cdr xs) (cons (car xs) ys))))

(define (fresh-name sym) (labeled-name sym (mvector '#())))
(define-syntax let-open
  (syntax-rules ()
    ((_ (name ...) body ...) (let ((name (syntax-open name)) ...) body ...))))
(define-syntax let-fresh
  (syntax-rules ()
    ((_ (name ...) body ...) (let ((name (syntax-open (fresh-name 'name))) ...)
                               body ...))))
(define-syntax define-fresh
  (syntax-rules ()
    ((_ name ...) (begin (define name (syntax-open (fresh-name 'name))) ...))))

;;; Language extension
(define-syntax define-syntax-parser*
  (syntax-rules ()
    ((_ e f (common* ...)) '())
    ((_ e f (common* ...) (name (c* ...)) rest ...)
     (cons (cons 'name (lambda (e f) common* ...
                         (match-syntax e f c* ...
                           (_ (error "invalid syntax:" f)))))
           (define-syntax-parser* e f (common* ...) rest ...)))))
(define-syntax define-syntax-transformer*
  (syntax-rules ()
    ((_ e-local e f) '())
    ((_ e-local e f (name (c* ...)) rest ...)
     (cons (cons 'name (lambda (e f)
                         (syntax-close
                           e-local (match-syntax e f c* ...
                                     (_ (error "invalid syntax:" f))))))
           (define-syntax-transformer* e-local e f rest ...)))))

;;; Mostly standard Scheme definitions
(define ($b*->body env body)
  (lambda (b*) (env-bind*! env b*) (expand-body* env body)))

(define (expand-letrec env p* a?* v* expand-body)
  (define ast-true (ast-literal #t))
  (define uninitialized* (map (lambda (_) ast-true) p*))
  (define (pbody b*)
    (env-bind*! env b*)
    (let ((e* (map (lambda (v) (expand env v)) v*)))
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
    (expand-letrec denv p* a* v* (lambda (env) (expand env final))))  )

(define (expand-define* env body)
  (when (not (list? body)) (error "invalid definition body:" body))
  (let outer-loop ((top body) (pending '()))
    (match top
      (`(,original-form . ,body-rest)
        (let loop ((form original-form))
          (match-syntax env form
            (`(begin ,@e*) (outer-loop e* (cons body-rest pending)))
            (`(define ,n ,def-body)
              (guard (name? n))
              (define b (cons n (car (param*->addr* (list n)))))
              (env-bind*! env (list b))
              (cons (cons b def-body) (outer-loop body-rest pending)))
            (`(define (,n . ,p*) . ,def-body)
              (let-open (n p* def-body)
                (loop (syntax-close
                        env-scheme `(define ,n (lambda ,p* . ,def-body))))))
            (`(begin . ,_) (error "invalid begin syntax:" form))
            (`(define . ,_) (error "invalid define syntax:" form))
            (_ (cond ((form->transformer env form)
                      => (lambda (t) (loop (t env form))))
                     (else (cons (cons (cons #f #f) original-form)
                                 (outer-loop body-rest pending))))))))
      ('() (match pending
             (`(,top . ,pending) (outer-loop top pending))
             ('() '()))))))

(define env-scheme (env-extend env-empty))

(env-bind-parser*!
  env-scheme
  (define-syntax-parser*
    env form
    ((define (loop d) (expand env d))
     (define (loop-close d) (loop (syntax-close env-scheme d))))
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
    (let ((`(let ,name ,b* . ,body)
            (guard (or (name? name)))
            (assert-binding* b*)
            (let-open (name body)
              (define p* (syntax-open (map car b*)))
              (define v* (syntax-open (map cadr b*)))
              (loop-close `(letrec ((,name (lambda ,p* . ,body)))
                             (,name . ,v*)))))
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
    ))

(env-bind-transformer*!
  env-scheme
  (define-syntax-transformer*
    env-scheme env form
    (letrec* ((`(letrec* ,b* . ,body)
                (let-open (b* body) `(letrec ,b* . ,body)))))
    (let* ((`(let* () . ,body) (let-open (body) `(let () . ,body)))
           (`(let* (,b . ,b*) . ,body)
             (let-open (b b* body) `(let (,b) (let* ,b* . ,body))))))
    (cond ((`(cond) `((quote ,(syntax-open 'error:cond:no-matching-clause))))
           (`(cond (else ,@body)) (let-open (body) `(let () . ,body)))
           (`(cond (,e) . ,cs)
             (let-open (e cs) (define-fresh t)
               `(let ((,t ,e)) (if ,t ,t (cond . ,cs)))))
           (`(cond (,e => ,proc) . ,cs)
             (let-open (e cs proc) (define-fresh t)
               `(let ((,t ,e)) (if ,t (,proc ,t) (cond . ,cs)))))
           (`(cond (,e ,@body) . ,cs)
             (let-open (e cs body) (define-fresh t)
               `(let ((,t ,e)) (if ,t (let () . ,body) (cond . ,cs)))))))
    (and ((`(and) #t)
          (`(and ,e) (syntax-open e))
          (`(and ,e . ,e*) (let-open (e e*) `(if ,e (and . ,e*) #f)))))
    (or ((`(or) #f)
         (`(or ,e) (syntax-open e))
         (`(or ,e . ,e*) (let-open (e e*) (define-fresh t)
                           `(let ((,t ,e)) (if ,t ,t (or . ,e*)))))))
    (when ((`(when ,c . ,body)
             (let-open (c body) `(if ,c (let () . ,body) #t)))))
    (unless ((`(unless ,c . ,body)
               (let-open (c body) `(if ,c #t (let () . ,body))))))))

(env-bind-parser*!
  env-scheme
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arity (length (cadr po-desc)))
         (cons name
               (lambda (env form)
                 (match-syntax env form
                   (`(,_ . ,a*)
                     (guard (list? a*) (= arity (length a*)))
                     (ast-primitive-op
                       name (map (lambda (a) (expand env a)) a*)))
                   (_ (error "invalid primitive op:" name arity form))))))
       primitive-ops))

(define (scheme-eval p) (eval-ast (expand env-scheme p)))

(define (scheme-extend binding-prefix)
  (let* ((tag (make-continuation-prompt-tag))
         (e (lambda (env) (set! env-scheme env)
              (expand env-scheme (shift-at tag k k))))
         (continue scheme-eval))
    (set! scheme-eval
      (reset-at tag (continue (append binding-prefix (list (expander e))))))))

(define primitive-op-procs
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define (x i) (string->symbol (string-append "x" (number->string i))))
         (define p* (map x (range (length (cadr po-desc)))))
         `(,name (lambda ,p* (,name . ,p*))))
       primitive-ops))

(scheme-extend `(let ,primitive-op-procs))

(scheme-extend
  '(letrec ((cons* (lambda (x xs) (if (null? xs) x
                                    (cons x (cons* (car xs) (cdr xs)))))))))

(scheme-extend '(let ((apply (lambda (f x . xs) (apply f (cons* x xs)))))))

(scheme-extend
  '(letrec
     ((not (lambda (b) (if b #f #t)))
      (vector->list (lambda (v)
                      (let loop ((i (- (vector-length v) 1)) (xs '()))
                        (if (< i 0) xs
                          (loop (- i 1) (cons (vector-ref v i) xs))))))
      (equal? (lambda (a b)
                (cond ((pair? a) (and (pair? b)
                                      (equal? (car a) (car b))
                                      (equal? (cdr a) (cdr b))))
                      ((vector? a) (and (vector? b) (equal? (vector->list a)
                                                            (vector->list b))))
                      ((tagged? a)
                       (and (tagged? b)
                            (equal? (tagged-tag a) (tagged-tag b))
                            (equal? (tagged-value a) (tagged-value b))))
                      (else (and (not (pair? b)) (not (vector? b))
                                 (not (tagged? b)) (eqv? a b))))))
      (vector (lambda xs (list->vector xs)))
      (list?  (lambda (v) (or (and (pair? v) (list? (cdr v))) (null? v))))
      (list   (lambda xs xs))
      (list*  (lambda (x . xs) (cons* x xs)))
      (foldl  (lambda (f acc xs) (if (null? xs) acc
                                   (foldl f (f (car xs) acc) (cdr xs)))))
      (foldr  (lambda (f acc xs) (if (null? xs) acc
                                   (f (car xs) (foldr f acc (cdr xs))))))
      (map (lambda (f xs . xss)
             (define (map1 f xs) (if (null? xs) '()
                                   (cons (f (car xs)) (map1 f (cdr xs)))))
             (cond ((null? xs) '())
                   (else (cons (apply f (car xs) (map1 car xss))
                               (apply map f (cdr xs) (map1 cdr xss)))))))
      (andmap (lambda (f xs . xss)
                (let loop ((last #t) (xs xs) (xss xss))
                  (and last (if (null? xs) last
                              (loop (apply f (car xs) (map car xss))
                                    (cdr xs) (map cdr xss)))))))
      (ormap (lambda (f xs . xss)
               (cond ((null? xs) #f)
                     ((apply f (car xs) (map car xss)) => (lambda (y) y))
                     (else (apply ormap f (cdr xs) (map cdr xss))))))
      (filter (lambda (p? xs)
                (cond ((null? xs) '())
                      ((p? (car xs)) (cons (car xs) (filter p? (cdr xs))))
                      (else (filter p? (cdr xs))))))
      (filter-not (lambda (p? xs) (filter (lambda (x) (not (p? x))) xs)))
      (length (lambda (xs) (foldl (lambda (_ l) (+ 1 l)) 0 xs)))
      (append (lambda (xs ys) (foldr cons ys xs)))
      (reverse-append (lambda (xs ys) (foldl cons ys xs)))
      (reverse (lambda (xs) (reverse-append xs '())))
      (take (lambda (xs n) (if (= 0 n) '()
                             (cons (car xs) (take (cdr xs) (- n 1))))))
      (drop (lambda (xs n) (if (= 0 n) xs (drop (cdr xs) (- n 1)))))
      (member (lambda (v xs) (cond ((null? xs) #f)
                                   ((equal? v (car xs)) xs)
                                   (else (member v (cdr xs))))))
      (caar (lambda (v) (car (car v))))
      (assoc (lambda (k xs) (cond ((null? xs) #f)
                                  ((equal? k (caar xs)) (car xs))
                                  (else (assoc k (cdr xs))))))
      )))

(env-bind-transformer*!
  env-scheme
  (define-syntax-transformer*
    env-scheme env form
    (quasiquote
      ((`(quasiquote ,qqf)
         (define (bad msg) (error "malformed quasiquote:" msg form))
         (define (build-pair a d) `(cons ,a ,d))
         (define (build-l->v xs) `(list->vector ,xs))
         (define (build-append xs ys) `(append ,xs ,ys))
         (define (tag t e)
           (build-pair `(quote ,(syntax-open t)) (build-pair e '(quote ()))))
         (let loop ((level 0) (qqf qqf))
           (match-syntax env qqf
             (`(quasiquote ,qq) (tag 'quasiquote (loop (+ level 1) qq)))
             (`(,'unquote ,uq) (if (= 0 level) (syntax-open uq)
                                 (tag 'unquote (loop (- level 1) uq))))
             (`((,'unquote-splicing ,uqs) . ,qq)
               (define qqd (loop level qq))
               (if (= 0 level) (build-append (syntax-open uqs) qqd)
                 `(,(tag 'unquote-splicing (loop (- level 1) uqs)) . ,qqd)))

             (`(quasiquote . ,x)         (bad 'quasiquote))
             ('unquote                   (bad 'unquote))
             (`(,'unquote . ,_)          (bad 'unquote))
             ('unquote-splicing          (bad 'unquote-splicing))
             (`(,'unquote-splicing . ,_) (bad 'unquote-splicing))

             (`#(,@vqq)      (build-l->v (loop level vqq)))
             (`(,qqa . ,qqd) (build-pair (loop level qqa) (loop level qqd)))
             (atom (guard (atom? atom)) `(quote ,(syntax-open atom)))
             (_ (error "invalid quasiquote datum:" qqf)))))))
    (case
      ((`(case ,scrutinee ,@clause*)
         (let-open (scrutinee) (define-fresh x)
           (define cond-body
             (let loop ((c* clause*))
               (match-syntax env c*
                 ('()                                '())
                 (`((else . ,body)) (let-open (body) `((else . ,body))))
                 (`(((,@data) . ,body) . ,cs)
                   (let-open (body data) (define-fresh d)
                     (define test `(ormap (lambda (,d) (equal? ,x ,d)) ',data))
                     (cons `(,test . ,body) (loop cs))))
                 (_ (error "invalid case clauses:" c*)))))
           `(let ((,x ,scrutinee)) (cond . ,cond-body))))))

    (match/==
      ((`(match/== ,==?-e ,scrutinee . ,body)
         (define (sclose stx) (syntax-close env-scheme stx))
         (define (cq p) (list (sclose 'quote) p))
         (define (cqq p) (list (sclose 'quasiquote) p))
         (define (cuq p) (list (sclose 'unquote) p))
         (define (ccons a d) (list (sclose 'cons) a d))
         (define (clist* ps) (cons (sclose 'list*) ps))
         (let-open (==?-e scrutinee) (define-fresh ==? x k-fail)
           (define (loop-clause c)
             (match-syntax env c
               (`(,pat (guard ,@conditions) ,@body)
                 (define succeed
                   (let-open (conditions body)
                     `(if (and . ,conditions) (let () . ,body) (,k-fail))))
                 (let loop-pat ((pat pat) (x x) (succeed succeed))
                   (match-syntax env pat
                     ('_ succeed)
                     (id (guard (name? id))
                         `(let ((,(syntax-open id) ,x)) ,succeed))
                     (`(quote ,datum) `(if (,==? ,x ',(syntax-open datum))
                                         ,succeed (,k-fail)))
                     ((list 'quasiquote qq)
                      (match-syntax env qq
                        ((list 'unquote pat)
                         (loop-pat pat x succeed))
                        ((list (list 'unquote-splicing id))
                         (let-open (id)
                           `(let ((,id ,x))
                              (if (list? ,id) ,succeed (,k-fail)))))
                        ((cons (list 'unquote-splicing id) _)
                         (error "unquote-splicing pattern must be last"))
                        (`#(,@vqq)
                          (define-fresh x-list)
                          `(if (vector? ,x)
                             (let ((,x-list (vector->list ,x)))
                               ,(loop-pat (cqq vqq) x-list succeed))
                             (,k-fail)))
                        (`(,qqa . ,qqd)
                          (loop-pat (ccons (cqq qqa) (cqq qqd)) x succeed))
                        (atom (guard (atom? atom))
                              (loop-pat (cq atom) x succeed))
                        (_ (error "invalid quasiquote pattern:" qq))))
                     (`(cons ,pa ,pd)
                       (define-fresh a d)
                       `(if (pair? ,x)
                          (let ((,a (car ,x)) (,d (cdr ,x)))
                            ,(loop-pat pa a (loop-pat pd d succeed)))
                          (,k-fail)))
                     (`(list* ,pat) (loop-pat pat x succeed))
                     (`(list* ,p0 . ,p*)
                       (loop-pat (ccons p0 (clist* p*)) x succeed))
                     (`(list ,@p*)
                       (loop-pat (clist* (append p* `(,(cq '())))) x succeed))
                     (`(vector ,@p*)
                       (loop-pat (cqq (list->vector (map cuq p*))) x succeed))
                     (lit (guard (literal? lit)) (loop-pat (cq lit) x succeed))
                     (_ (error "invalid pattern:" pat)))))
               (`(,pat . ,body)
                 (loop-clause `(,pat (,(sclose 'guard)) . ,body)))))
           (define (loop-outer c*)
             (if (null? c*)
               `((quote ,(syntax-open 'error:match:no-matching-clause)) ,x)
               `(let ((,k-fail (lambda () ,(loop-outer (cdr c*)))))
                  ,(loop-clause (car c*)))))
           (define match-let-body
             (match-syntax env body
               (`(,name ,@clause*) (guard (name? name))
                                   `(let ,(syntax-open name) ((,x ,scrutinee))
                                      ,(loop-outer clause*)))
               (`(,@clause*) `(let ((,x ,scrutinee)) ,(loop-outer clause*)))))
           `(let ((,==? ,==?-e)) ,match-let-body)))))
    (match ((`(match . ,body) (let-open (body) `(match/== equal? . ,body)))))
    ;; TODO:
    ;; define-type
    ;; let-alias, define-alias
    ;; unlet, unlet-except, undefine
    ))

(define firewall-bindings  ;; Support programs that set! a stdlib definition.
  (let ((names (map car (filter (lambda (kv) (eqv? 'lexical (cdr kv)))
                                (env-bound env-scheme)))))
    (map (lambda (n) (list n n)) names)))

(scheme-extend `(let ,firewall-bindings))
