(provide nscheme:expand)

(require box tagged-vector? tagged-vector?!
         assoc:empty assoc-ref assoc-set assoc-filter
         primitive-ops
         ast:quote ast:var ast:set! ast:if ast:apply ast:apply* ast:lambda
         ast:reset ast:shift ast:error ast:primitive-op)

;; Names
(define lname:tag         (box 'name))
(define (lname sym label) (vector lname:tag sym label))
(define (lname? d)        (tagged-vector? lname:tag '(symbol label) d))
(define (lname?! d)       (tagged-vector?! lname:tag '(symbol label) d))
(define (lname-sym d)     (import-apply (import (symbol) symbol) (lname?! d)))
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
(define (address:var name)           (address name atag:var-mutable))
(define (address:var-constant name)  (address name atag:var-constant))
(define (address:unbound? a)      (equal? (address-value a) atag:unbound))
(define (address:var-mutable? a)  (equal? (address-value a) atag:var-mutable))
(define (address:var-constant? a) (equal? (address-value a) atag:var-constant))
(define (address:var? a)          (or (address:var-mutable? a)
                                      (address:var-constant? a)))

(define env:empty            assoc:empty)
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
(define (expander? d)   (tagged-vector? expander:tag '(proc) d))
(define (literal? d) (or (boolean? d) (number? d) (char? d)))
(define (form->parser env form)
  (define n (if (pair? form) (car form) form))
  (and (name? n) (env-ref/syntax-parser? env n)))
(define (expand:apply env proc a*)
  (ast:apply (expand env proc) (map (lambda (a) (expand env a)) a*)))
(define (expand env form)
  (let loop ((form form))
    (cond ((form->parser env form) => (lambda (p) (p env form)))
          ((expander? form) => (import->lambda (import (proc) (proc env))))
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
(define ($thunk body) (ast:lambda #f '() body))
(define ($lambda/temp temp-name $temp->body)
  (define temp (fresh-name temp-name))
  (define (body b*) ($temp->body (expand (env-extend*/var env:empty b*) temp)))
  ($lambda #f (list temp) body))
(define ($let/temp temp-name $v $temp->body)
  (ast:apply ($lambda/temp temp-name $temp->body) (list $v)))
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
  ($let p?* uninitialized* b?*->body))

;; High-level expansion
(define (b?*->expand:body* env body*)
  (lambda (b?*) (expand:body* (env-extend*/var env b?*) body*)))
(define (expand:lambda env ~p?* body*)
  (define p* (~list->list ~p?*)) (param*?! p*)
  ($lambda (improper-list? ~p?*) p* (b?*->expand:body* env body*)))
(define (expand:let env p?* e* body*)
  ($let p?* (expand* env e*) (b?*->expand:body* env body*)))
(define (expand:let/temp temp-name env e $temp->body)
  ($let/temp temp-name (expand env e) $temp->body))
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
          (match/=? (make-syntax=? env:primitive-syntax env) form
            (`(begin ,@e*) (body*->rdef* e* rdef*))
            (`(define ,n ,body) (guard (name? n)) ($define n body rdef*))
            (`(define (,n . ,~p?*) . ,def-body*)
              (guard (name? n))
              (define (edef env) (expand:lambda env ~p?* def-body*))
              ($define n (expander edef) rdef*))
            (`(begin . ,_)  (error '"invalid begin:" form))
            (`(define . ,_) (error '"invalid define:" form))
            (_ ($define #f form rdef*)))) rdef* body*)))
  (when (null? rdef*) (error '"body cannot be empty:" body*))
  (define p?e* (reverse (cdr rdef*)))
  (define final-def (car rdef*))
  (define body-final (cdr final-def))
  (when (car final-def) (error '"body cannot end with a definition:" body*))
  (expand:letrec env (map car p?e*) (map cdr p?e*)
                 (lambda (env) (expand env body-final))))
(define (expand:error env a*) (ast:error (expand* env a*)))
(define (expand:and* env e*)
  (define re* (reverse e*))
  (if (null? re*) $true
    (foldr (lambda (e r) (ast:if (expand env e) r $false))
           (expand env (car re*)) (reverse (cdr re*)))))
(define (expand:or2 env e $rest)
  (expand:let/temp 'temp env e (lambda ($temp) (ast:if $temp $temp $rest))))

;; Bindings for variables
(define primitive-op-procs
  (map (lambda (po-desc)
         (define (x i) (string-append 'x (number->string i)))
         (define p* (map x (range (length (cadr po-desc)))))
         `(,(car po-desc) (lambda ,p* (,(car po-desc) . ,p*)))) primitive-ops))

(define derived-op-procs
  '((not (lambda (b) (if b #f #t)))
    (list->vector (lambda (xs)
                    (define result (make-mvector (length xs) #t))
                    (foldl (lambda (x i) (mvector-set! result i x) (+ i 1))
                           0 xs)
                    (mvector->vector result)))
    (vector->list (lambda (v)
                    (let loop ((i (- (vector-length v) 1)) (xs '()))
                      (if (< i 0) xs
                        (loop (- i 1) (cons (vector-ref v i) xs))))))
    (equal? (lambda (a b)
              (cond ((pair? a)    (and (pair? b) (equal? (car a) (car b))
                                       (equal? (cdr a) (cdr b))))
                    ((vector? a)  (and (vector? b) (equal? (vector->list a)
                                                           (vector->list b))))
                    ((boolean? a) (and (boolean? b) (boolean=? a b)))
                    ((char?    a) (and (char?    b) (char=?    a b)))
                    ((string?  a) (and (string?  b) (string=?  a b)))
                    ((mvector? a) (and (mvector? b) (mvector=? a b)))
                    ((number?  a) (and (number?  b) (number=?  a b)))
                    ((null? a)    (null? b))
                    ((procedure? a)
                     (and (procedure? b)
                          (error '"equal? undefined for two procedures:" a b)))
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

,(
(define (parser-descs->b* descs)
  (cons 'list
        (map (lambda (name&clause*)
               `(cons ',(car name&clause*)
                      (lambda (env form)
                        (define (ex form) (expand env form))
                        (match/=? (make-syntax=? env:primitive-syntax env) form
                          ,@(cdr name&clause*)
                          (_ (error '"invalid syntax:" form)))))) descs)))

;; Parsers for primitive syntax (no var dependencies)
(define parsers:primitive
  '((apply (`(,_ ,p ,a)    (ast:apply* (ex p) (ex a))))
    (quote (`(,_ ,datum)   (ast:quote datum)))
    (if    (`(,_ ,c ,t ,f) (ast:if (ex c) (ex t) (ex f))))
    (set!  (`(,_ ,name ,v) (guard (name? name))
                           (ast:set! (env-ref/var-mutable env name) (ex v))))
    (error (`(,_ ,@a*)     (expand:error env a*)))
    (reset (`(,_ ,@e*)     (ast:reset (expand:body* env e*))))
    (shift (`(,_ ,k ,@e*)  (ast:shift ($lambda #f (list k)
                                               (b?*->expand:body* env e*)))))
    (lambda (`(,_ ,~p?* ,@e*) (expand:lambda env ~p?* e*)))
    (letrec (`(,_ ,b* ,@e*)
              (binding*?! b*)
              (expand:letrec env (map car b*) (map cadr b*)
                             (lambda (env) (expand:body* env e*)))))
    (let (`(,_ ,name ,b* . ,e*)
           (guard (name? name))
           (binding*?! b*)
           (define (ex-proc env) (expand:lambda env (map car b*) e*))
           (expand:letrec env (list name) (list (expander ex-proc))
                          (lambda (env)
                            (expand:apply env name (map cadr b*)))))
      (`(,_ ,b* ,@e*) (binding*?! b*)
                      (expand:let env (map car b*) (map cadr b*) e*)))
    (let* (`(,_ ,b* ,@e*)
            (binding*?! b*)
            (let loop ((b* b*) (env env))
              (define (next b?*) (loop (cdr b*) (env-extend*/var env b?*)))
              (cond ((null? b*) (expand:body* env e*))
                    ((pair? b*) ($let (list (caar b*))
                                      (list (expand env (cadar b*))) next))))))
    (begin (`(,_ ,@e*) (guard (pair? e*))
                       (define rb* (reverse (map ex e*)))
                       ($begin (reverse (cdr rb*)) (car rb*))))
    (cond
      (`(,_ ,@clause*)
        (guard (pair? clause*))
        (let loop ((c* clause*))
          (match/=? (make-syntax=? env:primitive-syntax env) c*
            (`((else ,@e*))      (expand:body* env e*))
            (`((else . ,_) . ,_) (error '"invalid else clause in cond:" form))
            (`((,e) . ,c*)       (expand:or2 env e (loop c*)))
            (`((,e => ,p) . ,c*)
              (define ($t->body $t)
                (ast:if $t (ast:apply (ex p) (list $t)) (loop c*)))
              (expand:let/temp 'temp env e $t->body))
            (`((,e ,@e*) . ,c*)
              (ast:if (ex e) (expand:body* env e*) (loop c*)))
            ('() (ast:error (list (ast:quote '"no matching cond clause:")
                                  (ast:quote form))))
            (_ (error '"invalid cond:" form))))))
    (and (`(,_ ,@e*) (expand:and* env e*)))
    (or (`(,_ ,@e*)  (foldr (lambda (e r) (expand:or2 env e r)) $false e*)))
    (when (`(,_ ,c ,@e*)   (ast:if (ex c) (expand:body* env e*) $true)))
    (unless (`(,_ ,c ,@e*) (ast:if (ex c) $true (expand:body* env e*))))))

;; TODO:
;; let-alias, let-without|unlet, let-only|unlet-except[/var][/syntax][/all]
;; import, export, and/let*

;; Parsers with dependencies on base definitions
(define ($cons a d)       (ast:primitive-op 'cons (list a d)))
(define ($pair? x)        (ast:primitive-op 'pair? (list x)))
(define ($car x)          (ast:primitive-op 'car (list x)))
(define ($cdr x)          (ast:primitive-op 'cdr (list x)))
(define ($vector? x)      (ast:primitive-op 'vector? (list x)))
(define ($equal? a b)     (ast:apply $var:equal? (list a b)))
(define ($list? x)        (ast:apply $var:list?  (list x)))
(define ($append a b)     (ast:apply $var:append (list a b)))
(define ($list->vector x) (ast:apply $var:list->vector (list x)))
(define ($vector->list x) (ast:apply $var:vector->list (list x)))

(define (parse:match/=? env =?-e scrutinee body* full-form)
  (define (parse:clause $=? $x env pat rhs $k-fail)
    (define $fail (ast:apply $k-fail '()))
    (define ($try $c $k) (ast:if $c $k $fail))
    (define ($succeed env)
      (match/=? (make-syntax=? env:base env) rhs
        (`((guard ,@conds) . ,rhs)
          (guard (pair? rhs))
          ($try (expand:and* env conds) (expand:body* env rhs)))
        (`((guard . ,_) . ,_) (error '"invalid match rhs:" rhs))
        (_ (expand:body* env rhs))))
    (define (tqq datum) (list 'quasiquote datum))
    (let loop (($x $x) (p pat) (env env) ($succeed $succeed))
      (define ($try-vec pat)
        (define (k $x) (loop $x pat env $succeed))
        ($try ($vector? $x) ($let/temp 'x ($vector->list $x) k)))
      (match p retry
        ('_                            ($succeed env))
        (id         (guard (name? id)) (retry `(var ,p)))
        (`(var ,id) (guard (name? id))
                    ($let (list id) (list $x)
                          (lambda (b*) ($succeed (env-extend*/var env b*)))))
        (`(quote ,datum) ($try (ast:apply $=? (list $x (ast:quote datum)))
                               ($succeed env)))
        (`(cons ,pa ,pd)
          (define ($k env) ($let/temp 'x-cdr ($cdr $x)
                                      (lambda ($x) (loop $x pd env $succeed))))
          ($try ($pair? $x) ($let/temp 'x-car ($car $x)
                                       (lambda ($x) (loop $x pa env $k)))))
        (`(list* ,pat)     (retry pat))
        (`(list* ,p . ,p*) (retry `(cons ,p (list* ,p*))))
        (`(list ,@p*)      (retry `(list* ,(append p* (list '())))))
        (`(vector ,@p*)    ($try-vec `(list . ,p*)))
        ((list 'quasiquote qq)
         (match qq
           ((list (list 'unquote-splicing id)) (guard (name? id))
                                               ($try ($list? $x) (retry id)))
           ((list 'unquote p) (retry p))
           (`(,qa . ,qd)      (retry `(cons ,(tqq qa) ,(tqq qd))))
           (`#(,@qqv)         ($try-vec (tqq qqv)))
           ('unquote          (error '"bad unquote:" pat))
           ('unquote-splicing (error '"bad unquote-splicing:" pat))
           (datum             (retry `(quote ,datum)))))
        (lit (guard (or (boolean? pat) (number? pat) (char? pat)))
             (retry `(quote ,lit)))
        (_ (error '"invalid pattern:" pat)))))
  (define (parse:clauses $=? env c*)
    ($lambda/temp
      'scrutinee
      (lambda ($x)
        (foldr (lambda (c $fail)
                 (match c
                   (`(,pat ,@rhs)
                     (expand:let/temp
                       'k-fail (list (expander (lambda _ ($thunk $fail))))
                       (lambda ($kf) (parse:clause $=? $x env pat rhs $kf))))
                   (_ (error '"invalid match clause:" c))))
               (ast:error (list (ast:quote '"no matching clause:")
                                $x (ast:quote full-form))) c*))))
  (expand:let/temp
    '=? env =?-e
    (lambda ($=?)
      (ast:apply
        (match body*
          (`(,name . ,clause*)
            (guard (name? name))
            (define (e env) (parse:clauses $=? env clause*))
            (expand:letrec env (list name) (list (expander e))
                           (lambda (env) (env-ref/var env name))))
          (clause* (parse:clauses $=? env clause*)))
        (expand env scrutinee)))))

(define parsers:base
  '((case
      (`(,_ ,scrutinee ,@clause*)
        (guard (pair? clause*))
        (expand:let/temp
          'scrutinee env scrutinee
          (lambda (x)
            (match/=? (make-syntax=? env:base env) clause* loop
              (`((else ,@e*)) (expand:body* env e*))
              (`((else . ,_) . ,_)
                (error '"invalid else clause in case:" form))
              (`(((,@data) ,@e*) . ,c*)
                (ast:if (foldr (lambda (d r) (ast:if ($equal? x d) $true r))
                               $false (map ast:quote data))
                        (expand:body* env e*) (loop c*)))
              ('() (ast:error (list (ast:quote '"no matching case clause:")
                                    x (ast:quote form))))
              (c* (error '"invalid case clauses:" c*)))))))
    (quasiquote
      (`(,_ ,qqf)
        (define (bad msg) (error '"malformed quasiquote:" msg form))
        (define (tag t e) ($cons (ast:quote t) ($cons e (ast:quote '()))))
        (let loop ((level 0) (qqf qqf))
          (match/=? (make-syntax=? env:base env) qqf
            (`(quasiquote ,qq) (tag 'quasiquote (loop (+ level 1) qq)))
            (`(,'unquote ,uq) (if (= 0 level) (ex eq)
                                (tag 'unquote (loop (- level 1) uq))))
            (`((,'unquote-splicing ,uqs) . ,qq)
              (define qqd (loop level qq))
              (if (= 0 level) ($append (ex uqs) qqd)
                ($cons (tag 'unquote-splicing (loop (- level 1) uqs)) qqd)))
            (`(quasiquote . ,x)         (bad 'quasiquote))
            ('unquote                   (bad 'unquote))
            (`(,'unquote . ,_)          (bad 'unquote))
            ('unquote-splicing          (bad 'unquote-splicing))
            (`(,'unquote-splicing . ,_) (bad 'unquote-splicing))
            (`#(,@vqq)      ($list->vector (loop level vqq)))
            (`(,qqa . ,qqd) ($cons (loop level qqa) (loop level qqd)))
            (datum          (ast:quote datum))
            (_              (error "invalid quasiquote template:" qqf))))))
    (match/=?
      (`(,_ ,=?-e ,scrutinee ,@body)
        (parse:match/=? env =?-e scrutinee body form)))
    (match
      (`(,_ ,scrutinee ,@body)
        (parse:match/=? env (expander (lambda _ $var:equal?))
                        scrutinee body form)))))

;; Environment construction
`(begin
   (define env:primitive-syntax
     (env-extend*/syntax
       (env-extend*/syntax env:empty ,(parser-descs->b* parsers:primitive))
       (map (lambda (po-desc)
              (define name (car po-desc))
              (define arity (length (cadr po-desc)))
              (cons name
                    (lambda (env form)
                      ,'(match form
                          (`(,_ ,@a*) (guard (= arity (length a*)))
                                      (ast:primitive-op name (expand* env a*)))
                          (_ (error '"invalid primitive op:" po-desc form))))))
            primitive-ops)))

   (define env:base #t)
   (define nscheme:expand
     (reset (expand
              env:primitive-syntax
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

   (define $var:equal?       (env-ref/var env:base 'equal?))
   (define $var:list?        (env-ref/var env:base 'list?))
   (define $var:append       (env-ref/var env:base 'append))
   (define $var:list->vector (env-ref/var env:base 'list->vector))
   (define $var:vector->list (env-ref/var env:base 'vector->list))

   (set! env:base (env-extend*/syntax
                    env:base ,(parser-descs->b* parsers:base)))
   ))
