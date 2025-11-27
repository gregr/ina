;;;;;;;;;;;
;;; LLL ;;;
;;;;;;;;;;;
;; Statement ::= (begin Statement ...)
;;             | (set! Location Expr)
;; Expr      ::= Constant | Location | (Binary-op Expr Expr)
;; Binary-op ::= + | - | *
;; Location  ::= Var
;; Var       ::= <symbol>
;; Constant  ::= <integer>
(splicing-local
  ((define binop=>procedure `((+ . ,+) (- . ,-) (* . ,*))))

  (define (LLL-validate P)
    (define (Location? x) (symbol? x))
    (define (Constant? x) (integer? x))
    (define (Binary-op? x) (and (pair? x) (assv (car x) binop=>procedure)
                                (or (list? x) (mistake "not a list" x))
                                (apply (case-lambda ((a b) (and (Expr?! a) (Expr?! b)))
                                                    (_ (mistake "operator arity mismatch" x)))
                                       (cdr x))))
    (define (Expr? x) (or (Location? x) (Constant? x) (Binary-op? x)))
    (define (Expr?! x) (or (Expr? x) (mistake "not an expression" x)))
    (let loop ((S P))
      (apply (case (car S)
               ((set!) (lambda (lhs rhs)
                         (unless (Location? lhs) (mistake "not a location" lhs))
                         (unless (Expr? rhs) (mistake "not an expression" rhs))))
               ((begin) (lambda S* (for-each loop S*)))
               (else (mistake "not a Statement" S)))
             (cdr S))))

  (define (LLL-eval P v=>x)
    (mlet ((v=>x v=>x))
      (define (var-ref r) (let ((entry (assv r v=>x)))
                            (unless entry (mistake "unassigned variable" r))
                            (cdr entry)))
      (define (var-set! r x) (set! v=>x (cons (cons r x) (aremv r v=>x))))
      (define (Binary-op op a b) ((cdr (assv op binop=>procedure)) (Expr a) (Expr b)))
      (define (Expr x) (cond ((symbol? x) (var-ref x))
                             ((integer? x) x)
                             (else (apply (lambda (a b) (Binary-op (car x) a b)) (cdr x)))))
      (define (Assign lhs rhs) (var-set! lhs (Expr rhs)))
      (let loop ((S P))
        (apply (case (car S)
                 ((set!) Assign)
                 ((begin) (lambda S* (for-each loop S*)))
                 (else (mistake "not a Statement" S)))
               (cdr S)))
      v=>x)))
