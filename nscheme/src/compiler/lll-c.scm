;;;;;;;;;;;;;;;;;
;;; LLL for C ;;;
;;;;;;;;;;;;;;;;;
;; Statement ::= (begin Statement ...)
;;             | (set! Location Expr)
;; Expr      ::= Constant | Location | (Binary-op Expr Expr)
;; Binary-op ::= + | - | *
;; Location  ::= Var
;; Var       ::= <symbol>
;; Constant  ::= S64
;; S64       ::= <signed 64-bit integer>
(define (LLL-validate-C P)
  (define (Location? x) (symbol? x))
  (define (Constant? x) (and (integer? x) (or (<= -9223372036854775808 x 9223372036854775807)
                                              (mistake "not a signed 64-bit integer" x))))
  (define (Binary-op? x) (and (pair? x) (list? x) (memv (car x) '(+ - *))
                              (apply (case-lambda ((a b) (and (Expr? a) (Expr? b))) (_ #f))
                                     (cdr x))))
  (define (Expr? x) (or (Location? x) (Constant? x) (Binary-op? x)))
  (let loop ((S P))
    (apply (case (car S)
             ((set!) (lambda (lhs rhs)
                       (unless (Location? lhs) (mistake "not a location" lhs))
                       (unless (Expr? rhs) (mistake "not an expression" rhs))))
             ((begin) (lambda S* (for-each loop S*)))
             (else (mistake "not a Statement" S)))
           (cdr S))))

(define (LLL-emit-C P)
  (define emit (let ((out (current-output-port))) (lambda (line) (display line out))))
  (define (Location x) (and (symbol? x) (symbol->string x)))
  (define (Expr x)
    (define (Subexpr x) (if (pair? x) (string-append "(" (Expr x) ")") (Expr x)))
    (cond ((Location x))
          ((integer? x) (string-append (number->string x) "L"))
          (else (string-append (Subexpr (cadr x)) " " (symbol->string (car x)) " "
                               (Subexpr (caddr x))))))
  (define (Assign lhs rhs) (string-append (Location lhs) " = " (Expr rhs) ";\n"))
  (let loop ((S P))
    (apply
     (case (car S)
       ((set!) (lambda (lhs rhs) (emit (Assign lhs rhs))))
       ((begin) (lambda S* (for-each loop S*)))
       (else (mistake "not a Statement" S)))
     (cdr S))))
