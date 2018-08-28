(provide eval-ast test!)

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

;;; Evaluation
(define (closure->procedure variadic? addr* body cenv)
  (define (continue a*)
    (define b?* (map (lambda (addr a) (and addr (cons addr a))) addr* a*))
    (define env (env-extend* cenv (filter-not not b?*)))
    (eval-ast/env env body))
  (define plen (length addr*))
  (cond (variadic?
          (lambda arg*
            (when (< (length arg*) (- plen 1))
              (error '"too few arguments:" (- plen 1) (length arg*) clo arg*))
            (let ((a0* (take arg* (- plen 1))) (a1* (drop arg* (- plen 1))))
              (continue (append a0* (list a1*))))))
        (else (lambda arg*
                (when (not (= (length arg*) plen))
                  (error '"arity mismatch:" plen (length arg*) clo arg*))
                (continue arg*)))))

(define (eval-ast/env env tm)
  (define (loop tm) (eval-ast/env env tm))
  (match tm
    (`#(quote ,datum)         datum)
    (`#(var ,address)         (env-ref env address))
    (`#(set! ,address ,tm)    (env-set! env address (loop tm)))
    (`#(if ,c ,t ,f)          (if (loop c) (loop t) (loop f)))
    (`#(lambda ,v? ,a* ,body) (closure->procedure v? a* body env))
    (`#(apply ,p ,a*)         (apply (loop p) (map loop a*)))
    (`#(apply* ,p ,a)         (apply (loop p) (loop a)))
    (`#(reset ,body)          (reset (loop body)))
    (`#(shift ,proc)          (shift k ((loop proc) k)))
    (`#(error ,a*)            (apply error (map loop a*)))
    (`#(prim-op ,name ,a*)
      (define (invalid-op . _) (error '"invalid primitive op:" name))
      (define op (assoc-ref primitive-op-evaluators name invalid-op))
      (op (map loop a*)))
    (_ (error '"unknown term:" tm))))

(define (eval-ast tm) (eval-ast/env env-empty tm))

(define (test! test)
  (test 'quote
    (eval-ast '#(quote 7))
    7)
  (test 'if-1
    (eval-ast '#(if #(quote #t) #(quote 1) #(quote 2)))
    1)
  (test 'if-2
    (eval-ast '#(if #(quote #f) #(quote 1) #(quote 2)))
    2))
