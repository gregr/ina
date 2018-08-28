(provide
  ast-quote
  ast-var
  ast-set!
  ast-if
  ast-apply
  ast-apply*
  ast-lambda
  ast-reset
  ast-shift
  ast-error
  ast-primitive-op
  test!)

(require
  assoc-empty assoc-ref assoc-set
  primitive-ops)

;;; Runtime environments
(define env-empty assoc-empty)
(define (env-extend* env b*)
  (foldl (lambda (b e) (assoc-set e (car b) (make-mvector 1 (cdr b)))) env b*))
(define (env-ref-box env addr)
  (define box (assoc-ref env addr #f))
  (or box (error '"unbound address:" addr)))
(define (env-ref env addr)    (mvector-ref (env-ref-box env addr) 0))
(define (env-set! env addr v) (mvector-set! (env-ref-box env addr) 0 v))

;;; Primitive operations
(define primitive-op-procs
  `((mvector?        . ,mvector?)
    (vector?         . ,vector?)
    (pair?           . ,pair?)
    (null?           . ,null?)
    (string?         . ,string?)
    (char?           . ,char?)
    (number?         . ,number?)
    (integer?        . ,integer?)
    (boolean?        . ,boolean?)
    (procedure?      . ,procedure?)
    (boolean=?       . ,boolean=?)
    (char=?          . ,char=?)
    (string=?        . ,string=?)
    (mvector=?       . ,mvector=?)
    (char->integer   . ,char->integer)
    (integer->char   . ,integer->char)
    (string->vector  . ,string->vector)
    (vector->string  . ,vector->string)
    (cons            . ,cons)
    (car             . ,car)
    (cdr             . ,cdr)
    (vector-ref      . ,vector-ref)
    (vector-length   . ,vector-length)
    (make-mvector    . ,make-mvector)
    (mvector->vector . ,mvector->vector)
    (mvector-set!    . ,mvector-set!)
    (mvector-ref     . ,mvector-ref)
    (mvector-length  . ,mvector-length)
    (=               . ,=)
    (<=              . ,<=)
    (<               . ,<)
    (+               . ,+)
    (*               . ,*)
    (-               . ,-)
    (/               . ,/)
    ))

(define primitive-op-evaluators
  (map (lambda (po-desc)
         (define name (car po-desc))
         (define arg-sig (cadr po-desc))
         (define return-sig (caddr po-desc))  ;; TODO: validate return type?
         (define op (assoc-ref primitive-op-procs name #f))
         (define (valid? a*)
           (andmap (lambda (ty? a) (or (not ty?) (ty? a))) arg-sig a*))
         (define (full-op a*)
           (if (valid? a*) (apply op a*)
             (error '"primitive op type error:" name arg-sig a*)))
         (cons name full-op)) primitive-ops))

(define (ap* env ast*) (map (lambda (tm) (tm env)) ast*))

(define (ast-quote datum) (lambda (env) datum))
(define (ast-var address) (lambda (env) (env-ref env address)))
(define (ast-set! addr v) (lambda (env) (env-set! env address (v env))))
(define (ast-if c t f)    (lambda (env) (if (c env) (t env) (f env))))
(define (ast-apply proc arg*) (lambda (env) (apply (proc env) (ap* env arg*))))
(define (ast-apply* proc arg) (lambda (env) (apply (proc env) (arg env))))
(define (ast-reset body) (lambda (env) (reset (body env))))
(define (ast-shift proc) (lambda (env) (shift k ((proc env) k))))
(define (ast-error a*)   (lambda (env) (apply error (ap* env a*))))

(define (ast-primitive-op name a*)
  (define op (or (assoc-ref primitive-op-evaluators name #f)
                 (error '"invalid primitive op:" name)))
  (lambda (env) (op (ap* env a*))))

(define (ast-lambda variadic? address?* body)
  (define (continue cenv a*)
    (define b?* (map (lambda (addr a) (and addr (cons addr a))) addr* a*))
    (define env (env-extend* cenv (filter-not not b?*)))
    (body env))
  (define plen (length addr*))
  (cond (variadic?
          (lambda (env)
            (lambda a*
              (when (< (length a*) (- plen 1))
                (error '"too few arguments:" (- plen 1) (length a*) clo a*))
              (let ((a0* (take a* (- plen 1))) (a1* (drop a* (- plen 1))))
                (continue env (append a0* (list a1*)))))))
        (else (lambda (env)
                (lambda a*
                  (when (not (= (length a*) plen))
                    (error '"arity mismatch:" plen (length a*) clo a*))
                  (continue env a*))))))

(define (test! test)
  (test 'ast-quote
    ((ast-quote 7) env-empty)
    7)
  (test 'ast-if-1
    ((ast-if (ast-quote #t) (ast-quote 1) (ast-quote 2)) env-empty)
    1)
  (test 'ast-if-2
    ((ast-if (ast-quote #f) (ast-quote 1) (ast-quote 2)) env-empty)
    2))
