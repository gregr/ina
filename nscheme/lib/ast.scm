(provide primitive-op-descriptions
         primitive-op-type-signature primitive-op-handler
         ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:prim ast:context ast-eval test!)
(require param-bind)

(define (ast:quote datum)       (vector 'quote   datum))
(define (ast:var address)       (vector 'var     address))
(define (ast:set! param arg)    (vector 'set!    param arg))
(define (ast:if c t f)          (vector 'if      c t f))
(define (ast:apply proc arg)    (vector 'apply   proc arg))
(define (ast:lambda param body) (vector 'lambda  param body))
(define (ast:prim name a*)      (vector 'prim    name a*))
(define (ast:context name a*)   (vector 'context name a*))

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
  (map
    (lambda (po-desc)
      (define name (car po-desc))
      (define sig (primitive-op-type-signature po-desc))
      (define arg-sig (car sig))
      (define return-sig (cadr sig))  ;; TODO: validate return type?
      (define op (primitive-op-handler (assoc name primitive-op-descriptions)))
      (define (valid? a*)
        (andmap (lambda (ty? a)
                  (or (not ty?) ((cdr (assoc ty? type-predicates)) a)))
                arg-sig a*))
      (define (full-op a*)
        (if (valid? a*) (apply op a*)
          (error '"primitive op type error:" name arg-sig a*)))
      (cons name full-op)) primitive-op-descriptions))

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
                            (lambda (env) (op (ex* env a*)))))
             ((? 'context)
              (let ((name (@ 1)) (a* (map ev (@ 2))))
                (define op (or (alist-get context-ops name #f)
                               (error '"invalid context op:" name)))
                (lambda (env) (apply op (map (lambda (a) (a env)) a*)))))
             (#t          (error '"unknown ast:" ast))))) env:empty))

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
