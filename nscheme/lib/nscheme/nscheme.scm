(provide nscheme:expand)

(require box tagged-vector? tagged-vector?!
         assoc-empty assoc-ref assoc-set assoc-filter
         primitive-ops
         ast:quote ast:var ast:set! ast:if ast:apply ast:apply* ast:lambda
         ast:reset ast:shift ast:error ast:primitive-op)

;; Names
(define lname:tag         (box 'name))
(define (lname sym label) (vector lname:tag sym label))
(define (lname? d)        (tagged-vector? lname:tag '(symbol label) d))
(define (lname?! d)       (tagged-vector?! lname:tag '(symbol label) d))
(define (lname-sym d)     (import/apply (import (symbol) symbol) (lname?! d)))
(define (make-fresh-name)
  (let ((label 0)) (lambda (n) (set! label (+ label 1))
                     (lname (if (string? n) n (lname-sym n)) label))))
(define fresh-name (make-fresh-name))
(define (name? n) (or (string? n) (lname? n)))
(define (name->string n)
  (cond ((lname? n)
         => (import->lambda
              (import (symbol label)
                (string-append symbol '"." (number->string label)))))
        (else n)))

;;; Syntactic environments
(define (address name value) (vector name value))
(define (address-name a)     (vector-ref a 0))
(define (address-value a)    (vector-ref a 1))
(define atag:unbound      'unbound)
(define atag:var-mutable  'var-mutable)
(define atag:var-constant 'var-constant)
(define (address:syntax name parser) (address name parser))
(define (address:unbound name)       (address name atag:unbound))
(define (address:unbound? a)         (equal? (address-value a) atag:unbound))
(define (address:var name)           (address name atag:var))
(define (address:var? a) (define v (address-value a))
  (or (equal? v atag:var-mutable) (equal? v atag:var-constant)))
(define (address:var-mutable? a) (equal? (address-value a) atag:var-mutable))

(define env:empty            assoc-empty)
(define (env-extend env b)   (assoc-set env (car b) (cdr b)))
(define (env-extend* env b*) (foldl (lambda (b e) (env-extend e b)) env b*))
(define (env-extend*/syntax env b*)
  (env-extend* env (map (lambda (b) (define n (car b)) (define parse (cdr b))
                          (cons n (address:syntax (fresh-name n) parse))) b*)))
(define (env-extend*/var env b?*)
  (env-extend* env (map (lambda (b) (cons (car b) (address:var (cdr b))))
                        (filter (lambda (b?) (car b?)) b?*))))
(define (env-ref/default env n default) (assoc-ref env n default))
(define (env-ref env n) (or (env-ref/default env n #f) (address:unbound n)))
(define (env-ref/syntax-parser? env n)
  (define p (address-value (env-ref env n)))
  (and (procedure? p) p))
(define (env-ref/var env n) (define a (env-ref env n))
  (if (address:var? a) (address-name a) (error '"unbound variable:" n)))
(define (env-ref/var-mutable env n)
  (define a (env-ref env n))
  (unless (address:var? a)         (error '"unbound variable:" n))
  (unless (address:var-mutable? a) (error '"immutable variable:" n))
  (address-name a))
(define (env-alias env n aliased)
  (env-extend env n (env-ref env aliased)))
(define (env-hide* env n*)
  (define (unbound n) (cons n (address:unbound (fresh-name n))))
  (env-extend* env (map unbound n*)))
(define (env-freeze* env n*)
  (define (frz n) (cons n (address:var-constant (env-ref/var env n))))
  (env-extend* env (map frz n*)))
(define (env-freeze*/mutable env)
  (define (mutable? b) (address:var-mutable? (cdr b)))
  (env-freeze* env (map car (filter mutable? env))))
(define (env-only env n*)
  (env-extend* env (map (lambda (n) (cons n (env-ref env n))) n*)))

(define (make-syntax=? env-a env-b)
  (define (=? a b)
    (cond ((and (pair? a) (pair? b)) (and (=? (car a) (car b))
                                          (=? (cdr a) (cdr b))))
          ((and (name? a) (name? b)) (equal? (address-name (env-ref env-a a))
                                             (address-name (env-ref env-b b))))
          (else (equal? a b))))
  =?)

;; Expansion
(define expander:tag    (box 'expander))
(define (expander proc) (vector expander:tag proc))
(define (expander? d)   (tagged-vector? expander:tag '(proc)))
(define (literal? d) (or (boolean? d) (number? d) (char? d)))
(define (form->parser env form)
  (define n (if (pair? form) (car form) form))
  (and (name? n) (env-ref/syntax-parser? env n)))
(define (expand:apply env proc a*)
  (ast:apply (expand env p) (map (lambda (a) (expand env a)) a*)))
(define (expand env form)
  (let loop ((form form))
    (cond ((form->parser env form) => (lambda (p) (p env form)))
          ((expander? form) ((expander-proc form) env))
          ((literal? form)  (ast:quote form))
          ((name? form)     (ast:var (env-ref/var env form)))
          (else (match form
                  (`(,p ,@a*) (expand:apply env p a*))
                  (_          (error '"invalid syntax:" form)))))))
(define (expand* env form*) (map (lambda (f) (expand env f)) form*))

;;; Parameters
(define (improper-list? d)
  (and (not (null? d)) (or (not (pair? d)) (improper-list? (cdr d)))))
(define (~list->list d)
  (cond ((null? d) '())
        ((pair? d) (cons (car d) (~list->list (cdr d))))
        (else (list d))))
(define (param? p) (or (not p) (name? p)))
(define (param*?! p*)
  (unless (andmap param? p*) (error '"invalid parameter list:" p*))
  (define n* (filter name? p*))
  (unless (= (length n*) (length (remove-duplicates n*)))
    (error '"parameter list contains duplicate names:" p*)))
(define (binding*?! b*)
  (define (binding? b) (and (list? b) (= 2 (length b))
                            (or (not (car b)) (name? (car b)))))
  (unless (and (list? b*) (andmap binding? b*))
    (error '"invalid binding list:" b*)))
(define (param*->addr* n*)
  (map (lambda (n) (and (name? n) (fresh-name n))) n*))

;;; High-level AST construction
(define $true (ast:quote #t))
(define $false (ast:quote #f))
(define ($lambda variadic? p?* b?*->body)
  (define a?* (param*->addr* p?*))
  (ast:lambda variadic? a?* (b?*->body (map cons p?* a?*))))
(define ($let p?* v* b?*->body) (ast:apply ($lambda #f p?* b?*->body) v*))
(define ($begin body-initial* body-final)
  (cond ((null? body-initial*) body-final)
        (else ($let '(#f) (list (car body-initial*))
                    (lambda (_) ($begin (cdr body-initial*) body-final))))))
(define ($letrec p?* b?*->v*&body)
  (define uninitialized* (map (lambda (_) $true) p?*))
  (define (b?*->body b?*)
    (let* ((v*b (b?*->v*&body b?*)) (v* (car v*b)) (body (cdr v*b)))
      (define a?* (map cdr b?*))
      ($begin (map (lambda (a? v) (if a? (ast:set! a? v) v)) a?* v*) body)))
  ($let p?* uninitialized* b*->body))

;; High-level expansion
(define (b?*->expand:body* env body*)
  (lambda (b?*) (expand:body* (env-extend*/var env b?*) body*)))
(define (expand:lambda env ~p?* body*)
  (define p* (~list->list ~p*)) (param*?! p*)
  ($lambda (improper-list? ~p*) p* (b?*->expand:body* env body*)))
(define (expand:let env p?* e* body*)
  ($let p?* (expand* env e*) (b?*->expand:body* env body*)))
(define (expand:let/temp env e $temp->body)
  (define temp (fresh-name 'temp))
  ($let (list temp) (list (expand env e))
        (lambda (b*) (define $temp (expand (env-extend*/var env b*) temp))
          ($temp->body $temp))))
(define (expand:letrec env p?* e* env->body)
  (define (b?*->v*&body b?*)
    (let ((env (env-extend*/var env b?*)))
      (cons (expand* env e*) (env->body env))))
  ($letrec p?* b?*->v*&body))
(define (expand:body* env body*)
  (define ($define n def-body rdef*) (cons (cons n def-body) rdef*))
  (define rdef*
    (let body*->rdef* ((body* body*) (rdef* '()))
      (unless (list? body*) (error '"body must be a list:" body*))
      (foldl
        (lambda (form rdef*)
          (match/=? (make-syntax=? env:base env) form
            (`(begin ,@e*) (body*->rdef* e* rdef*))
            (`(define ,n ,body) (guard (name? n)) ($define n body rdef*))
            (`(define (,n . ,~p?*) . ,def-body*)
              (guard (name? n))
              (define (edef env) (expand:lambda env ~p?* def-body*))
              ($define n (expander edef) rdef*))
            (`(begin . ,_)  (error '"invalid begin:" form))
            (`(define . ,_) (error '"invalid define:" form))
            (_ ($define #f form rdef*)))) rdef* body*)))
  (define rdef* (body*->rdef* env body*))
  (when (null? rdef*) (error '"body cannot be empty:" body*))
  (define p?e* (reverse (cdr rdef*)))
  (define final-def (car rdef*))
  (define body-final (cdr final-def))
  (when (car final-def) (error '"body cannot end with a definition:" body*))
  (expand:letrec env (map car p?e*) (map cdr p?e*)
                 (lambda (env) (expand env body-final))))
(define (expand:error env a*) (ast:error (expand* env a*)))
(define (expand:and env e $rest) ($if (expand env e1) $rest $false))
(define (expand:or env e $rest)
  (expand:let/temp env e (lambda ($temp) (ast:if $temp $temp $rest))))

;; Parsers for primitive syntax (no var dependencies)

;; For convenience, we can share the same parser for syntactic forms
;; defined at the same lexical level.  We determine the appropriate handler by
;; matching the form's name.
(define (expand:primitive-syntax env form)
  (define (ex form) (expand env form))
  (match/=? (make-syntax=? env:primitive-syntax env) form
    (`(apply ,p ,a) (ast:apply* (ex p) (ex a)))
    (`(quote ,datum) (ast:quote datum))
    (`(if ,c ,t ,f) (ast:if (ex c) (ex t) (ex f)))
    (`(set! ,name ,v) (guard (name? name))
                      (ast:set! (env-ref/var-mutable env name) (ex v)))
    (`(reset ,@body*) (ast:reset (expand:body* env body*)))
    (`(shift ,k ,@body*)
      (ast:shift ($lambda #f (list k) (b?*->expand:body* env body*))))
    (`(error ,@a*) (expand:error env a*))
    (`(lambda ,~p?* ,@body*) (expand:lambda env ~p?* body*))
    (`(letrec ,b* ,@body*)
      (binding*?! b*) (expand:letrec env (map car b*) (map cadr b*)
                                     (lambda (env) (expand:body* env body*))))
    (`(let ,name ,b* . ,body)
      (guard (name? name))
      (binding*?! b*)
      (define (ex-proc env) (expand:lambda env (map car b*) body))
      (expand:letrec env (list name) (list (expander ex-proc))
                     (lambda (env) (expand:apply env name (map cadr b*)))))
    (`(let ,b* ,@body*) (binding*?! b*)
                        (expand:let env (map car b*) (map cadr b*) body*))
    (`(let* ,b* ,@body*)
      (binding*?! b*)
      (let loop ((b* b*) (env env))
        (define (continue b?*) (loop (cdr b*) (env-extend*/var env b?*)))
        (cond ((null? b*) (expand:body* env body*))
              ((pair? b*) ($let (list (caar b*)) (list (expand env (cadar b*)))
                                continue)))))
    (`(begin ,@body*) (guard (pair? body*))
                      (define rb* (reverse (map ex body*)))
                      ($begin (reverse (cdr rb*)) (car rb*)))
    (`(cond ,@clause*)
      (guard (pair? clause*))
      (let loop ((c* clause*))
        (match/=? (make-syntax=? env:primitive-syntax env) c*
          (`((else ,@body*)) (expand:body* env body*))
          (`((else . ,_) . ,_) (error '"invalid else clause in cond:" form))
          (`((,e) . ,c*) (expand:or env e (loop c*)))
          (`((,e => ,p) . ,c*)
            (define ($t->body $t) (ast:if $t (ast:apply (ex p) $t) (loop c*)))
            (expand:let/temp env e $t->body))
          (`((,e ,@e*) . ,c*) (ast:if (ex e) (expand:body* env e*) (loop c*)))
          ('() (ast:error (list (ast:quote '"no matching cond clause:")
                                (ast:quote form))))
          (_ (error '"invalid cond:" form)))))
    (`(and ,@e*) (foldr (lambda (e rest) (expand:and env e rest)) $true e*))
    (`(or ,@e*)  (foldr (lambda (e rest) (expand:or env e rest)) $false e*))
    (`(when ,c ,@body*) (ast:if (ex c) (expand:body* env body*) $true))
    (`(unless ,c ,@body*) (ast:if (ex c) $true (expand:body* env body*)))
    (_ (error '"invalid syntax:" form))))

;; TODO:
;; let-alias, let-without|unlet, let-only|unlet-except[/var][/syntax][/all]

(define env:primitive-syntax
  (env-extend*/syntax
    (env-extend*/syntax
      env:empty (map (lambda (sname) (cons sname expand:primitive-syntax))
                     '(apply quote if set! lambda letrec let begin reset shift
                             letrec* let* cond and or when unless)))
    (map (lambda (po-desc)
           (define name (car po-desc)) (define arity (length (cadr po-desc)))
           (cons name (lambda (env form)
                        (match form
                          (`(,_ ,@a*) (guard (= arity (length a*)))
                                      (ast:primitive-op name (expand* env a*)))
                          (_ (error '"invalid primitive op:" po-desc form))))))
         primitive-ops)))

;; Bindings for variables
(define primitive-op-procs
  (map (lambda (po-desc)
         (define (x i) (string-append 'x (number->string i)))
         (define p* (map x (range (length (cadr po-desc)))))
         `(,name (lambda ,p* (,(car po-desc) . ,p*)))) primitive-ops))

(define derived-op-procs
  '((not (lambda (b) (if b #f #t)))
    (vector->list (lambda (v)
                    (let loop ((i (- (vector-length v) 1)) (xs '()))
                      (if (< i 0) xs
                        (loop (- i 1) (cons (vector-ref v i) xs))))))
    (equal? (lambda (a b)
              (when (ormap procedure? (list a b))
                (error '"cannot use equal? on procedures:" a b))
              (cond ((pair? a) (and (pair? b) (equal? (car a) (car b))
                                    (equal? (cdr a) (cdr b))))
                    ((vector? a) (and (vector? b) (equal? (vector->list a)
                                                          (vector->list b))))
                    ((boolean? a) (and (boolean? b) (boolean=? a b)))
                    ((char?    a) (and (char?    b) (char=?    a b)))
                    ((string?  a) (and (string?  b) (string=?  a b)))
                    ((mvector? a) (and (mvector? b) (mvector=? a b)))
                    ((number?  a) (and (number?  b) (number=?  a b)))
                    ((null? a)    (null? b))
                    (else (error '"invalid type for equal?:" a b)))))
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
    ;; TODO: range
    (take (lambda (xs n) (if (= 0 n) '()
                           (cons (car xs) (take (cdr xs) (- n 1))))))
    (drop (lambda (xs n) (if (= 0 n) xs (drop (cdr xs) (- n 1)))))
    (member (lambda (v xs) (cond ((null? xs) #f)
                                 ((equal? v (car xs)) xs)
                                 (else (member v (cdr xs))))))
    (caar (lambda (v) (car (car v))))
    (assoc (lambda (k xs) (cond ((null? xs) #f)
                                ((equal? k (caar xs)) (car xs))
                                (else (assoc k (cdr xs))))))))

(define env:base #t)
(define nscheme:expand
  (reset (expand env:primitive-syntax
                 `(let ,primitive-op-procs
                    (letrec ((cons* (lambda (x xs)
                                      (if (null? xs) x
                                        (cons x (cons* (car xs) (cdr xs)))))))
                      (let ((apply (lambda (f x . xs) (apply f (cons* x xs)))))
                        (letrec ,derived-op-procs
                          ,(expander
                             (lambda (env)
                               (set! env:base (env-freeze*/mutable env))
                               (expand env:base (shift k k)))))))))))

;; Parsers with dependencies on: append, equal?
;; TODO: quasiquote, case, match (var patterns to match _ ids)
;(set! env:base
  ;(env-extend*/syntax
    ;env:base

    ;))