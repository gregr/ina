;;;;;;;;;;;;;;;;;
;;; LLL for C ;;;
;;;;;;;;;;;;;;;;;
(define (LLL-emit-C P)
  (define emit (let ((out (current-output-port))) (lambda (line) (display line out))))
  (define (Location x) (if (symbol? x)
                           (symbol->string x)
                           (and (pair? x) (eqv? (car x) 'memory)
                                (let ((type (case (cadr x)
                                              ((1) "unsigned char")
                                              ((2) "unsigned short")
                                              ((4) "unsigned int")
                                              (else "long"))))
                                  (string-append "*(" type "*)(" (Expr (caddr x)) ")")))))
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
