(provide primitive-op-descriptions
         primitive-op-type-signature primitive-op-handler
         ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:prim ast:let ast:letrec ast:begin ast-eval test!
         ast-elaborate)
(require param-map param-bind)

;; Basic AST
(define (ast:quote datum)       (vector 'quote   datum))
(define (ast:var address)       (vector 'var     address))
(define (ast:set! param arg)    (vector 'set!    param arg))
(define (ast:if c t f)          (vector 'if      c t f))
(define (ast:apply proc arg)    (vector 'apply   proc arg))
(define (ast:lambda param body) (vector 'lambda  param body))
(define (ast:prim name a*)      (vector 'prim    name a*))

;; Extended AST
(define (ast:let n* a* body)    (vector 'let    n* a* body))
(define (ast:letrec n* l* body) (vector 'letrec n* l* body))
(define (ast:begin e* final)    (vector 'begin  e* final))

;; Dynamic context operations
(define context-ops
  (list (cons 'reset (lambda (proc) (reset (proc))))
        (cons 'shift (lambda (proc) (shift k (proc k))))))

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
      (list '=     =  '((number? number?) boolean?))
      (list '<=    <= '((number? number?) boolean?))
      (list '<     <  '((number? number?) boolean?))
      (list '>=    >= '((number? number?) boolean?))
      (list '>     >  '((number? number?) boolean?))
      (list '+     +  '((number? number?) number?))
      (list '*     *  '((number? number?) number?))
      (list '-     -  '((number? number?) number?))
      (list '/     /  '((number? number?) number?))

      (list 'truncate truncate '((number?) integer?))

      ;bitwise-and
      ;bitwise-ior
      ;bitwise-xor
      ;bitwise-not
      ;bitwise-bit-set?
      ;bitwise-bit-field
      ;arithmetic-shift
      ;integer-length
      )))

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
  (define malformed (filter-not valid-primop? primitive-op-descriptions))
  (unless (null? malformed)
    (error '"malformed primitive-op-descriptions:" malformed)))

(define primitive-ops
  (append
    (map (lambda (po-desc)
           (define name (car po-desc))
           (define sig (primitive-op-type-signature po-desc))
           (define arg-sig (car sig))
           (define return-sig (cadr sig))  ;; TODO: validate return type?
           (define op (primitive-op-handler po-desc))
           (define (valid? a*)
             (andmap (lambda (ty? a)
                       (or (not ty?) ((cdr (assoc ty? type-predicates)) a)))
                     arg-sig a*))
           (define (full-op . a*)
             (if (valid? a*) (apply op a*)
               (error '"primitive op type error:" name arg-sig a*)))
           (cons name full-op)) primitive-op-descriptions)
    context-ops))

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
                            (define op (or (alist-get primitive-ops name #f)
                                           (error '"invalid primitive:" name)))
                            (lambda (env) (apply op (ex* env a*)))))
             ((? 'let)
              (let ((n* (@ 1)) (a* (map ev (@ 2))) (body (ev (@ 3))))
                (lambda (env)
                  (body (env-extend* env (param-bind n* (ex* env a*)))))))
             ((? 'letrec)
              (let ((n* (@ 1)) (l* (@ 2)) (body (@ 3)))
                (ev (ast:let n* (map (lambda #f (ast:quote #t)) n*)
                             (ast:begin (map ast:set! n* l*) body)))))
             ((? 'begin) (let ((e* (map ev (@ 1))) (final (ev (@ 2))))
                           (lambda (env) (ex* env e*) (final env))))
             (#t (error '"unknown ast:" ast))))) env:empty))

(define (test! test)
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
    (let ((i 0)) (lambda (n)
                   (define name (mvector-ref n 0))
                   (unless (pair? name)
                     (define id (string-append (natural->string i) '"." name))
                     (set! i (+ i 1)) (mvector-set! n 0 (cons id name)))
                   (car (mvector-ref n 0)))))
  (define (param-elaborate bind param/names arg rest)
    (define param (param-map name->id param/names))
    (define nid (let ((i -1)) (lambda () (set! i (+ i 1))
                                (string-append 'p. (natural->string i)))))
    (define (fail p a) (ast:apply (ast:var 'fail) (list (ast:quote p) a)))
    (define ast:fail
      (ast:lambda
        (list 'p 'a)
        (ast:apply (ast:quote '"parameter/argument mismatch:")
                   (list (ast:quote param) arg (ast:var 'p) (ast:var 'a)))))
    (ast:let
      (list 'fail) (list ast:fail)
      (let loop ((p param) (a arg) (rest rest))
        (define (prim name)   (ast:prim name (list a)))
        (define (test ? body) (ast:if ? body (fail p a)))
        (cond ((pair? p)
               (let ((icar (nid)) (icdr (nid)))
                 (test (prim 'pair?)
                   (ast:let (list icar icdr) (list (prim 'car) (prim 'cdr))
                            (loop (car p) (ast:var icar)
                                  (loop (cdr p) (ast:var icdr) rest))))))
              ((vector? p)
               (let ((ivl (nid)))
                 (test (prim 'vector?)
                   (ast:let (list ivl) (list (prim 'vector->list))
                            (loop (vector->list p) (ast:var ivl) rest)))))
              ((null? p) (test (prim 'null?) rest))
              ((not p)   rest)
              (#t        (bind p a rest))))))
  (let ev ((ast ast))
    (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
    (cond ((? 'var    ) (ast:var (name->id (@ 1))))
          ((? 'set!   ) (ast:let (list 'a.set!) (list (ev (@ 2)))
                                 (param-elaborate
                                   (lambda (p a rest)
                                     (ast:begin (list (ast:set! p a)) rest))
                                   (@ 1) (ast:var 'a.set!) (ast:quote #t))))
          ((? 'if     ) (ast:if (ev (@ 1)) (ev (@ 2)) (ev (@ 3))))
          ((? 'apply  ) (ast:apply (ev (@ 1)) (list (ev (@ 2)))))
          ((? 'lambda ) (ast:lambda (list 'a.lambda)
                                    (param-elaborate
                                      (lambda (p a rest)
                                        (ast:let (list p) (list a) rest))
                                      (@ 1) (ast:var 'a.lambda) (ev (@ 2)))))
          ((? 'prim   ) (ast:prim    (@ 1) (map ev (@ 2))))
          ((? 'begin  ) (ast:begin (map ev (@ 1)) (ev (@ 2))))
          ((? 'let    ) (ast:let
                          (map name->id (@ 1)) (map ev (@ 2)) (ev (@ 3))))
          ((? 'letrec ) (ast:letrec
                          (map name->id (@ 1)) (map ev (@ 2)) (ev (@ 3))))
          (#t           ast))))
