(provide
  ast:quote
  ast:var
  ast:set!
  ast:if
  ast:apply
  ast:lambda
  ast:reset
  ast:shift
  ast:primitive-op
  eval/ast
  test!)

(require type-predicates primitive-ops)

;; Runtime environments
(define env:empty '())
(define (env-extend* env b*)
  (foldl (lambda (b e) (cons (cons (car b) (make-mvector 1 (cdr b))) e)) env b*))
(define (env-ref-box env addr)
  (define rib (assoc addr env))
  (if rib (cdr rib) (error '"unbound address:" addr)))
(define (env-ref env addr)    (mvector-ref (env-ref-box env addr) 0))
(define (env-set! env addr v) (mvector-set! (env-ref-box env addr) 0 v))

;; Primitive operations
(define primitive-op-procs
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

(define primitive-op-evaluators
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arg-sig (cadr po-desc))
         (define return-sig (caddr po-desc))  ;; TODO: validate return type?
         (define op (cdr (assoc name primitive-op-procs)))
         (define (valid? a*)
           (andmap (lambda (ty? a)
                     (or (not ty?) ((cdr (assoc ty? type-predicates)) a)))
                   arg-sig a*))
         (define (full-op a*)
           (if (valid? a*) (apply op a*)
             (error '"primitive op type error:" name arg-sig a*)))
         (cons name full-op)) primitive-ops))

(define (ast:quote datum)    (lambda (env) datum))
(define (ast:var address)    (lambda (env) (env-ref env address)))
(define (ast:set! addr v)    (lambda (env) (env-set! env addr (v env))))
(define (ast:if c t f)       (lambda (env) (if (c env) (t env) (f env))))
(define (ast:apply proc arg) (lambda (env) (apply (proc env) (arg env))))
(define (ast:reset body)     (lambda (env) (reset (body env))))
(define (ast:shift proc)     (lambda (env) (shift k ((proc env) k))))

(define (ast:lambda variadic? addr* body)
  (define (~length ~xs) (if (pair? ~xs) (+ 1 (~length (cdr ~xs))) 0))
  (define (continue cenv a*)
    (define b?* (map (lambda (addr a) (and addr (cons addr a))) addr* a*))
    (define env (env-extend* cenv (filter-not not b?*)))
    (body env))
  (define plen (length addr*))
  (cond (variadic?
          (lambda (env)
            (lambda a*
              (when (< (~length a*) (- plen 1))
                (error '"too few arguments:" (- plen 1) (~length a*) a*))
              (let ((a0* (take a* (- plen 1))) (a1* (drop a* (- plen 1))))
                (continue env (append a0* (list a1*)))))))
        (else (lambda (env)
                (lambda a*
                  (unless (and (list? a*) (= (length a*) plen))
                    (error '"arity mismatch:" plen (length a*) a*))
                  (continue env a*))))))

(define (ast:primitive-op name a*)
  (define op (cdr (or (assoc name primitive-op-evaluators)
                      (error '"invalid primitive op:" name))))
  (lambda (env) (op (map (lambda (a) (a env)) a*))))

(define (eval/ast ast)
  ((let loop ((ast ast))
     (define (@ i) (vector-ref ast i)) (define (? tag) (equal? (@ 0) tag))
     (if (procedure? ast) ast
       (cond ((? 'quote)   (ast:quote (@ 1)))
             ((? 'var)     (ast:var (@ 1)))
             ((? 'set!)    (ast:set! (@ 1) (loop (@ 2))))
             ((? 'if)      (ast:if (loop (@ 1)) (loop (@ 2)) (loop (@ 3))))
             ((? 'apply)   (ast:apply (loop (@ 1)) (loop (@ 2))))
             ((? 'lambda)  (ast:lambda (@ 1) (@ 2) (loop (@ 3))))
             ((? 'reset)   (ast:reset (loop (@ 1))))
             ((? 'shift)   (ast:shift (loop (@ 1))))
             ((? 'prim-op) (ast:primitive-op (@ 1) (map loop (@ 2))))
             (else (error '"unknown ast:" ast))))) env:empty))

(define (test! test)
  (test 'ast:quote
    ((ast:quote 7) env:empty)
    7)
  (test 'ast:if-1
    ((ast:if (ast:quote #t) (ast:quote 1) (ast:quote 2)) env:empty)
    1)
  (test 'ast:if-2
    ((ast:if (ast:quote #f) (ast:quote 1) (ast:quote 2)) env:empty)
    2)

  (test 'quote
    (eval/ast '#(quote 7))
    7)
  (test 'if-1
    (eval/ast '#(if #(quote #t) #(quote 1) #(quote 2)))
    1)
  (test 'if-2
    (eval/ast '#(if #(quote #f) #(quote 1) #(quote 2)))
    2))
