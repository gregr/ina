;;;;;;;;;;;
;;; LLL ;;;
;;;;;;;;;;;
;; Statement ::= (begin Statement ...)
;;             | (set! Location Expr)
;; Expr      ::= Constant | Location | (Binary-op Expr Expr)
;; Binary-op ::= + | - | *
;; Location  ::= Var | Memory
;; Var       ::= <symbol>
;; Memory    ::= (memory Width Expr)
;; Width     ::= 8
;; Constant  ::= <integer>
(splicing-local
  ((define binop=>procedure `((+ . ,+) (- . ,-) (* . ,*))))

  (define (LLL-validate P)
    (define (Memory? x) (and (pair? x) (eqv? (car x) 'memory) (list? (cdr x))
                             (apply (case-lambda
                                      ((width expr) (unless (eqv? width 8)
                                                      (mistake "invalid memory width" width))
                                                    (Expr?! expr))
                                      (_ (mistake "memory arity mismatch" x)))
                                    (cdr x))))
    (define (Location? x) (or (symbol? x) (Memory? x)))
    (define (Constant? x) (integer? x))
    (define (Binary-op? x) (and (pair? x) (assv (car x) binop=>procedure)
                                (or (list? (cdr x)) (mistake "not a list" x))
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

  (define (LLL-eval P loc=>x)
    (mlet ((loc=>x loc=>x))
      (define (Memory->addr x) (let ((width (cadr x)) (addr (Expr (caddr x))))
                                 (unless (= (integer-floor-mod addr width) 0)
                                   (mistake "unaligned memory address for width" width addr))
                                 addr))
      (define (loc-ref l) (let ((entry (assv l loc=>x)))
                            (unless entry (mistake "unassigned location" l))
                            (cdr entry)))
      (define (loc-set! l x) (set! loc=>x (cons (cons l x) (aremv l loc=>x))))
      (define (Binary-op op a b) ((cdr (assv op binop=>procedure)) (Expr a) (Expr b)))
      (define (Expr x) (cond ((symbol? x) (loc-ref x))
                             ((integer? x) x)
                             ((eqv? (car x) 'memory) (loc-ref (Memory->addr x)))
                             (else (apply (lambda (a b) (Binary-op (car x) a b)) (cdr x)))))
      (define (Assign lhs rhs) (loc-set! (if (symbol? lhs) lhs (Memory->addr lhs)) (Expr rhs)))
      (let loop ((S P))
        (apply (case (car S)
                 ((set!) Assign)
                 ((begin) (lambda S* (for-each loop S*)))
                 (else (mistake "not a Statement" S)))
               (cdr S)))
      (append (afilter symbol? loc=>x) (afilter integer? loc=>x)))))
