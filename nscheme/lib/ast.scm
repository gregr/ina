((provide type-predicates primitive-op-descriptions
          primitive-op-type-signature primitive-op-handler
          ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
          ast:prim astx:let astx:letrec astx:begin
          ast:null ast:true ast:false ast:cons ast:list ast:vector
          ast:apply* ast:let ast:begin ast:shift ast:reset
          ast-eval ast-elaborate)
 (require test param-map param-bind param-names name->string))

;; Basic AST
(define (ast:quote datum)       (vector 'quote   datum))
(define (ast:var address)       (vector 'var     address))
(define (ast:set! param arg)    (vector 'set!    param arg))
(define (ast:if c t f)          (vector 'if      c t f))
(define (ast:apply proc arg)    (vector 'apply   proc arg))
(define (ast:lambda param body) (vector 'lambda  param body))
(define (ast:prim name a*)      (vector 'prim    name a*))

;; Extended AST
(define (astx:let n* a* body)    (vector 'let    n* a* body))
(define (astx:letrec n* l* body) (vector 'letrec n* l* body))
(define (astx:begin e* final)    (vector 'begin  e* final))

;; High-level AST construction
(define ast:null        (ast:quote '()))
(define ast:true        (ast:quote #t))
(define ast:false       (ast:quote #f))
(define (ast:cons a d)  (ast:prim 'cons (list a d)))
(define (ast:list . xs) (foldr ast:cons ast:null xs))
(define (ast:vector . xs)
  (define vargs (list (ast:quote (length xs)) ast:true))
  (define mv (make-mvector 1 'mv)) (define $mv (ast:var mv))
  (define (! i x) (ast:prim 'mvector-set! (list $mv (ast:quote i) x)))
  (ast:let (list mv) (list (ast:prim 'make-mvector vargs))
           (ast:begin (append (map ! (range (length xs)) xs)
                              (list (ast:prim 'mvector->vector (list $mv)))))))
(define (ast:apply* $proc $a*) (ast:apply $proc (apply ast:list $a*)))
(define (ast:let p* v* body)   (ast:apply* (ast:lambda p* body) v*))
(define (ast:begin a*)
  (define ra* (reverse (cons ast:true a*)))
  (foldl (lambda (a rest) (ast:let '(#f) (list a) rest)) (car ra*) (cdr ra*)))
(define (ast:shift proc) (ast:prim 'shift (list proc)))
(define (ast:reset body) (ast:prim 'reset (list (ast:lambda '() body))))

;; Primitive operations
(define type-predicates
  (list (cons 'procedure? procedure?)
        (cons 'mvector?   mvector?)
        (cons 'vector?    vector?)
        (cons 'pair?      pair?)
        (cons 'null?      null?)
        (cons 'boolean?   boolean?)
        (cons 'string?    string?)
        (cons 'number?    number?)
        (cons 'integer?   integer?)
        (cons 'fixnum?    fixnum?)
        (cons 'flonum?    flonum?)))

(define context-op-descriptions
  (list (list 'reset (lambda (proc) (reset (proc)))     '((procedure?) #f))
        (list 'shift (lambda (proc) (shift k (proc k))) '((procedure?) #f))))

;; TODO: classify primitive behaviors in more detail, for use in analysis.
;; e.g., some possibilities: simple; allocation; mutable-read/write; context
(define primitive-op-descriptions
  (append
    (map (lambda (tp) (list (car tp) (cdr tp) '((#f) boolean?)))
         type-predicates)
    (list
      (list 'boolean=?   boolean=?   '((boolean? boolean?)     boolean?))
      (list 'number=?    number=?    '((number? number?)       boolean?))
      (list 'string=?    string=?    '((string? string?)       boolean?))
      (list 'mvector=?   mvector=?   '((mvector? mvector?)     boolean?))
      (list 'procedure=? procedure=? '((procedure? procedure?) boolean?))

      (list 'string->vector string->vector '((string?) vector?))
      (list 'vector->string vector->string '((vector?) string?))

      (list 'cons cons '((#f #f) pair?))
      (list 'car  car  '((pair?) #f))
      (list 'cdr  cdr  '((pair?) #f))

      (list 'vector-ref    vector-ref    '((vector? fixnum?) #f))
      (list 'vector-length vector-length '((vector?)         fixnum?))

      (list 'make-mvector    make-mvector    '((fixnum? #f)          mvector?))
      (list 'mvector->vector mvector->vector '((mvector?)            vector?))
      (list 'mvector-set!    mvector-set!    '((mvector? fixnum? #f) #t))
      (list 'mvector-ref     mvector-ref     '((mvector? fixnum?)    #f))
      (list 'mvector-length  mvector-length  '((mvector?)            fixnum?))

      ;; TODO: derive these.
      (list 'string<? string<? '((string? string?) boolean?))
      (list 'string>? string>? '((string? string?) boolean?))
      ;; TODO: flonum variants.
      (list '=  =  '((number? number?) boolean?))
      (list '<= <= '((number? number?) boolean?))
      (list '<  <  '((number? number?) boolean?))
      (list '>= >= '((number? number?) boolean?))
      (list '>  >  '((number? number?) boolean?))
      (list '+  +  '((number? number?) number?))
      (list '*  *  '((number? number?) number?))
      (list '-  -  '((number? number?) number?))
      (list '/  /  '((number? number?) number?))
      (list 'truncate         truncate         '((number?) integer?))
      (list 'arithmetic-shift arithmetic-shift '((integer? integer?) integer?))
      (list 'bitwise-and      bitwise-and      '((integer? integer?) integer?))
      (list 'bitwise-ior      bitwise-ior      '((integer? integer?) integer?))
      (list 'bitwise-xor      bitwise-xor      '((integer? integer?) integer?))
      (list 'bitwise-not      bitwise-not      '((integer?) integer?))
      (list 'integer-length   integer-length   '((integer?) integer?)))))

(define all-op-descriptions
  (append primitive-op-descriptions context-op-descriptions))

(define (primitive-op-handler        po-desc) (cadr po-desc))
(define (primitive-op-type-signature po-desc) (caddr po-desc))

;; Sanity check primitive-op-descriptions.
(let ()
  (define (valid-type-signature? ts)
    (define (valid-type? t)
      (or (equal? #t t) (not t) (assoc t type-predicates)))
    (and (list? ts) (= 2 (length ts)) (list? (car ts))
         (andmap valid-type? (car ts)) (valid-type? (cadr ts))))
  (define (valid-primop? op)
    (and (list? op) (= 3 (length op)) (string? (car op))
         (procedure? (primitive-op-handler op))
         (valid-type-signature? (primitive-op-type-signature op))))
  (define malformed (filter-not valid-primop? all-op-descriptions))
  (unless (null? malformed)
    (error '"malformed primitive-op-descriptions:" malformed)))

;; Runtime environments
(define env:empty '())
(define (env-extend* env b*)
  (foldl (lambda (b e)
           (define cell    (make-mvector 1 (cdr b)))
           (define (get)   (mvector-ref  cell 0))
           (define (set v) (mvector-set! cell 0 v))
           (cons (cons (car b) (cons get set)) e)) env b*))
(define (env-ref-capabilities env addr)
  (define rib (assoc addr env))
  (if rib (cdr rib) (error '"unbound address:" addr)))
(define (env-ref env addr)    ((car (env-ref-capabilities env addr))))
(define (env-set! env addr v) ((cdr (env-ref-capabilities env addr)) v))

(define (ast-eval ast)
  (define (ex* env c*) (map (lambda (c) (c env)) c*))
  ((let ev ((ast ast))
     (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
     (if (procedure? ast) ast
       (cond ((? 'quote)  (let ((datum (@ 1))) (lambda (env) datum)))
             ((? 'var)    (let ((n (@ 1)))     (lambda (env) (env-ref env n))))
             ((? 'set!)   (let ((param (@ 1)) (arg (ev (@ 2))))
                            (lambda (env)
                              (define (! b) (env-set! env (car b) (cdr b)))
                              (for-each ! (param-bind param (arg env))))))
             ((? 'if)     (let ((c (ev (@ 1))) (t (ev (@ 2))) (f (ev (@ 3))))
                            (lambda (env) (if (c env) (t env) (f env)))))
             ((? 'apply)  (let ((proc (ev (@ 1)))
                                (arg (if (vector? (@ 2)) (ev (@ 2))
                                       (let ((a* (map ev (@ 2))))
                                         (lambda (env) (ex* env a*))))))
                            (lambda (env) (apply (proc env) (arg env)))))
             ((? 'lambda) (let ((param (@ 1)) (body (ev (@ 2))))
                            (lambda (env) (lambda arg
                                            (define b* (param-bind param arg))
                                            (body (env-extend* env b*))))))
             ((? 'prim)   (let ((name (@ 1)) (a* (map ev (@ 2))))
                            (define op
                              (car (or (alist-get all-op-descriptions name #f)
                                       (error '"invalid primitive:" name))))
                            (lambda (env) (apply op (ex* env a*)))))
             ((? 'let)
              (let ((n* (@ 1)) (a* (map ev (@ 2))) (body (ev (@ 3))))
                (lambda (env)
                  (body (env-extend* env (param-bind n* (ex* env a*)))))))
             ((? 'letrec)
              (let ((n* (@ 1)) (l* (@ 2)) (body (@ 3)))
                (ev (astx:let n* (map (lambda #f (ast:quote #t)) n*)
                              (astx:begin (map ast:set! n* l*) body)))))
             ((? 'begin) (let ((e* (map ev (@ 1))) (final (ev (@ 2))))
                           (lambda (env) (ex* env e*) (final env))))
             (#t (error '"unknown ast:" ast))))) env:empty))

(when test
  (test 'quote
    (ast-eval '#(quote 7))
    7)
  (test 'if-1
    (ast-eval '#(if #(quote #t) #(quote 1) #(quote 2)))
    1)
  (test 'if-2
    (ast-eval '#(if #(quote #f) #(quote 1) #(quote 2)))
    2))

(define (natural->string/digits digits n)
  (define base (vector-length digits))
  (if (= n 0) (vector-ref digits 0)
    (let loop ((n n))
      (if (= n 0) '""
        (let ((quotient (truncate (/ n base))))
          (string-append (loop quotient)
                         (vector-ref digits (- n (* base quotient)))))))))

(define (natural->string n)
  (natural->string/digits '#("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") n))

(define (ast-elaborate ast)
  (define name->id
    (let ((i -1))
      (lambda (n) (set! i (+ i 1))
        (string-append (natural->string i) '"." (name->string n)))))
  (define (name*->id* n*) (map (lambda (n) (cons n (name->id n))) n*))
  (define (env-extend* env n* id*) (append (map cons n* id*) env))
  (define (env-ref env n)
    (let ((rib (assoc n env))) (if rib (cdr rib) (error '"unbound name:" n))))
  (let ev ((env '()) (ast ast))
    (define (param-elaborate bind env param/ids param/names arg body)
      (define param/strings (param-map name->string param/names))
      (define nid (let ((i -1)) (lambda () (set! i (+ i 1))
                                  (string-append 'p. (natural->string i)))))
      (define rest (ev env body))
      (define (fail p a) (ast:apply (ast:var 'fail) (list (ast:quote p) a)))
      (define ast:fail
        (ast:lambda (list 'p 'a)
                    (ast:apply (ast:quote '"parameter/argument mismatch:")
                               (list (ast:quote param/strings)
                                     arg (ast:var 'p) (ast:var 'a)))))
      (astx:let
        (list 'fail) (list ast:fail)
        (let loop ((p param/ids) (a arg) (rest rest))
          (define (prim name)   (ast:prim name (list a)))
          (define (test ? body) (ast:if ? body (fail p a)))
          (cond ((pair? p)
                 (let ((icar (nid)) (icdr (nid)))
                   (test (prim 'pair?)
                     (astx:let (list icar icdr) (list (prim 'car) (prim 'cdr))
                               (loop (car p) (ast:var icar)
                                     (loop (cdr p) (ast:var icdr) rest))))))
                ((vector? p)
                 (let ((ivl (nid)))
                   (test (prim 'vector?)
                     (astx:let (list ivl) (list (prim 'vector->list))
                               (loop (vector->list p) (ast:var ivl) rest)))))
                ((null? p) (test (prim 'null?) rest))
                ((not p)   rest)
                (#t        (bind p a rest))))))
    (define (ev* env a*) (map (lambda (a) (ev env a)) a*))
    (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
    (cond ((? 'var ) (ast:var (env-ref env (@ 1))))
          ((? 'set!)
           (define p/names (@ 1))
           (define p/ids   (param-map (lambda (n) (env-ref env n)) p/names))
           (astx:let
             (list 'a.set!) (list (ev env (@ 2)))
             (param-elaborate
               (lambda (p a rest) (astx:begin (list (ast:set! p a)) rest))
               env p/ids p/names (ast:var 'a.set!) (ast:quote #t))))
          ((? 'if   ) (ast:if (ev env (@ 1)) (ev env (@ 2)) (ev env (@ 3))))
          ((? 'apply) (define arg (if (vector? (@ 2)) (@ 2)
                                    (apply ast:list (@ 2))))
                      (ast:apply (ev env (@ 1)) (list (ev env arg))))
          ((? 'lambda)
           (define p/names (@ 1))
           (define p/ids   (param-map name->id p/names))
           (ast:lambda
             (list 'a.lambda)
             (param-elaborate
               (lambda (p a rest) (astx:let (list p) (list a) rest))
               (env-extend* env (param-names p/names) (param-names p/ids))
               p/ids p/names (ast:var 'a.lambda) (@ 2))))
          ((? 'prim  ) (ast:prim (@ 1) (ev* env (@ 2))))
          ((? 'begin ) (astx:begin (ev* env (@ 1)) (ev env (@ 2))))
          ((? 'let   ) (define i* (name*->id* (@ 1)))
                       (astx:let i* (ev* env (@ 2))
                                 (ev (env-extend* env (@ 1) i*) (@ 3))))
          ((? 'letrec) (define i* (name*->id* (@ 1)))
                       (astx:letrec i* (ev* env (@ 2))
                                    (ev (env-extend* env (@ 1) i*) (@ 3))))
          ((? 'quote ) ast)
          (#t          (error '"unknown ast:" ast)))))
