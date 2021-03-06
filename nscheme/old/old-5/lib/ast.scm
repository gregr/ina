((provide type-predicates primitive-op-descriptions
          primitive-op-type-signature primitive-op-handler
          ast:quote ast:var ast:set! ast:if ast:apply ast:lambda ast:prim
          ast:begin ast:let ast:letrec ast:apply* ast:begin1 ast:let*
          ast:null ast:true ast:false ast:cons ast:list ast:vector
          ast:shift ast:reset ast-eval ast-elaborate)
 (require test param-map param-bind param-names name->string))

;; Basic AST
(define (ast:quote datum)       (vector 'quote  datum))
(define (ast:var address)       (vector 'var    address))
(define (ast:set! param arg)    (vector 'set!   param arg))
(define (ast:if c t f)          (vector 'if     c t f))
(define (ast:apply proc arg)    (vector 'apply  proc arg))
(define (ast:lambda param body) (vector 'lambda param body))
(define (ast:prim name a*)      (vector 'prim   name a*))
(define (ast:begin  a* final)   (vector 'begin  a* final))
(define (ast:let    p* a* body) (vector 'let    p* a* body))
(define (ast:letrec p* a* body) (vector 'letrec p* a* body))

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
(define (ast:begin1 e*)
  (if (null? e*) ast:true
    (let (((suf . rpre) (split (reverse e*) 1)))
      (ast:begin (reverse rpre) (car suf)))))
(define (ast:let* p* v* body)
  (foldr (lambda ((p . v) body) (ast:let (list p) (list v) body))
         body (map cons p* v*)))
(define (ast:shift proc) (ast:prim 'shift (list proc)))
(define (ast:reset body) (ast:prim 'reset (list (ast:lambda '() body))))
(define (ast:begin/let ast)
  (define (@ i) (vector-ref ast i))
  (ast:let* (map (lambda #f #f) (@ 1)) (@ 1) (@ 2)))
(define (ast:let/lambda ast)
  (define (@ i) (vector-ref ast i))
  (ast:apply* (ast:lambda (@ 1) (@ 3)) (@ 2)))
(define (ast:letrec/set! ast)
  (define (@ i) (vector-ref ast i))
  (define n* (param-names (@ 1)))
  (ast:let n* (map (lambda #f ast:true) n*)
           (ast:begin (map ast:set! (@ 1) (@ 2)) (@ 3))))

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
             ((? 'apply)  (let ((proc (ev (@ 1))) (arg (ev (@ 2))))
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
             ((? 'let)    (ev (ast:let/lambda ast)))
             ((? 'letrec) (ev (ast:letrec/set! ast)))
             ((? 'begin)  (let ((e* (map ev (@ 1))) (final (ev (@ 2))))
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
  (define (env-extend* env n* id*) (append (map cons n* id*) env))
  (define (env-ref env n)
    (let ((rib (assoc n env)))
      (if rib (cdr rib) (error '"unbound name:" n (map car env)))))
  (let ev ((env '()) (ast ast))
    (define (ref n) (env-ref env n))
    (define (param-elaborate param/ids param/names arg body)
      (define param/strings (param-map name->string param/names))
      ;; TODO: use of param-apply restricts the scope of this elaboration.
      ;; Eventually, this entire elaboration should be replaced with this:
      ;; * convert ast:letrec into uses of set! (other translations are possible)
      ;; * elaborate set! param matching
      ;; * replace set! with mvector-set! so that all variables are immutable
      ;; * replace dynamic var and delimited control operators via CPS transform
      ;; * elaborate lambda param matching; avoid quadratic growth of error message constants
      ;; * lift lambda and pass closure argument explicitly
      ;;   * locally bind free variable names as closure-refs
      ;;   * replace lambda expressions with closure construction
      (ast:prim 'param-apply (list (ast:quote param/strings)
                                   arg (ast:lambda param/ids body))))
    (define (ev* env a*) (map (lambda (a) (ev env a)) a*))
    (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
    (cond ((? 'var ) (ast:var (ref (@ 1))))
          ((? 'set!)
           (define p/names (@ 1))
           (define p/ids   (map ref (param-names p/names)))
           (define p/alts  (map (lambda (n) (string-append 'a. n)) p/ids))
           (ast:let (list 'a.set!) (list (ev env (@ 2)))
                    (param-elaborate
                      p/alts p/names (ast:var 'a.set!)
                      (ast:begin (map ast:set! p/ids (map ast:var p/alts))
                                 ast:true))))
          ((? 'if   ) (ast:if (ev env (@ 1)) (ev env (@ 2)) (ev env (@ 3))))
          ((? 'apply) (define arg (if (vector? (@ 2)) (@ 2)
                                    (apply ast:list (@ 2))))
                      (ast:apply (ev env (@ 1)) (list (ev env arg))))
          ((? 'lambda)
           (define p/names (@ 1))
           (define p/ns    (param-names p/names))
           (define p/ids   (map name->id p/ns))
           (define benv    (env-extend* env p/ns p/ids))
           (ast:lambda (list 'a.lambda)
                       (param-elaborate
                         p/ids p/names (ast:var 'a.lambda) (ev benv (@ 2)))))
          ((? 'prim  ) (ast:prim (@ 1) (ev* env (@ 2))))
          ((? 'begin ) (ast:begin (ev* env (@ 1)) (ev env (@ 2))))
          ((? 'let   ) (ev env (ast:let/lambda ast)))
          ((? 'letrec) (ev env (ast:letrec/set! ast)))
          ((? 'quote ) ast)
          (#t          (error '"unknown ast:" ast)))))
