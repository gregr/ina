(provide ast:quote ast:var ast:set! ast:if ast:apply ast:lambda
         ast:reset ast:shift ast:prim ast-eval test!)
(require type-predicates primitive-op-descriptions)

(define (ast:quote datum)      (vector 'quote  datum))
(define (ast:var address)      (vector 'var    address))
(define (ast:set! addr value)  (vector 'set!   addr value))
(define (ast:if c t f)         (vector 'if     c t f))
(define (ast:apply proc arg)   (vector 'apply  proc arg))
(define (ast:reset body)       (vector 'reset  body))
(define (ast:shift proc)       (vector 'shift  proc))
(define (ast:lambda ~p?* body) (vector 'lambda ~p?* body))
(define (ast:prim name a*)     (vector 'prim   name a*))

;; Primitive operations
(define primitive-op-handlers
  (list (cons 'mvector?        mvector?)
        (cons 'vector?         vector?)
        (cons 'pair?           pair?)
        (cons 'null?           null?)
        (cons 'string?         string?)
        (cons 'number?         number?)
        (cons 'integer?        integer?)
        (cons 'boolean?        boolean?)
        (cons 'procedure?      procedure?)
        (cons 'boolean=?       boolean=?)
        (cons 'number=?        eqv?)
        (cons 'string=?        string=?)
        (cons 'mvector=?       mvector=?)
        (cons 'procedure=?     procedure=?)
        (cons 'string->vector  string->vector)
        (cons 'vector->string  vector->string)
        (cons 'cons            cons)
        (cons 'car             car)
        (cons 'cdr             cdr)
        (cons 'vector-ref      vector-ref)
        (cons 'vector-length   vector-length)
        (cons 'make-mvector    make-mvector)
        (cons 'mvector->vector mvector->vector)
        (cons 'mvector-set!    mvector-set!)
        (cons 'mvector-ref     mvector-ref)
        (cons 'mvector-length  mvector-length)
        (cons '=               =)
        (cons '<=              <=)
        (cons '<               <)
        (cons '+               +)
        (cons '*               *)
        (cons '-               -)
        (cons '/               /)))

(unless (= (length primitive-op-descriptions) (length primitive-op-handlers))
  (error '"mismatching primitive op handlers:"
         (map car primitive-op-handlers) (map car primitive-op-descriptions)))

(define primitive-ops
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arg-sig (cadr po-desc))
         (define return-sig (caddr po-desc))  ;; TODO: validate return type?
         (define op (cdr (assoc name primitive-op-handlers)))
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

;; TODO: import from interpreter.scm
(define (param-bind param arg)
  (let loop ((p param) (a arg))
    (cond ((and (pair? p) (pair? a)) (append (loop (car p) (car a))
                                             (loop (cdr p) (cdr a))))
          ((and (vector? p) (vector? a))
           (loop (vector->list p) (vector->list a)))
          ((string? p)               (list (cons p a)))
          ((and (null? p) (null? a)) '())
          ((not p)                   '())
          (else (error '"parameter/argument mismatch:" param arg p a)))))

(define (ast-eval ast)
  ((let ev ((ast ast))
     (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
     (if (procedure? ast) ast
       (cond ((? 'quote)  (let ((datum (@ 1))) (lambda (env) datum)))
             ((? 'var)    (let ((n (@ 1)))     (lambda (env) (env-ref env n))))
             ((? 'set!)   (let ((n (@ 1)) (v (ev (@ 2))))
                            (lambda (env) (env-set! env n (v env)))))
             ((? 'if)     (let ((c (ev (@ 1))) (t (ev (@ 2))) (f (ev (@ 3))))
                            (lambda (env) (if (c env) (t env) (f env)))))
             ((? 'apply)  (let ((proc (ev (@ 1))) (arg (ev (@ 2))))
                            (lambda (env) (apply (proc env) (arg env)))))
             ((? 'lambda) (let ((param (@ 1)) (body (ev (@ 2))))
                            (lambda (env) (lambda arg
                                            (define b* (param-bind param arg))
                                            (body (env-extend* env b*))))))
             ((? 'reset)  (let ((body (ev (@ 1))))
                            (lambda (env) (reset (body env)))))
             ((? 'shift)  (let ((proc (ev (@ 1))))
                            (lambda (env) (shift k ((proc env) k)))))
             ((? 'prim)   (let ((name (@ 1)) (a* (map ev (@ 2))))
                            (define op (or (alist-ref primitive-ops name #f)
                                           (error '"invalid primitive:" name)))
                            (lambda (env) (op (map (lambda (a) (a env)) a*)))))
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
