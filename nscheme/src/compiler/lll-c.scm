;;;;;;;;;;;;;;;;;
;;; LLL for C ;;;
;;;;;;;;;;;;;;;;;
(define LLL-C-types
  ##eos"typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef signed char s8;
typedef signed short s16;
typedef signed int s32;
typedef signed long long s64;
"eos##)

(define (LLL-emit-C P)
  (define u64-suffix "ull")
  (define emit (let ((out (current-output-port))) (lambda (line) (display line out))))
  (define Label? string?)
  (define (Location x) (if (symbol? x)
                           (symbol->string x)
                           (and (pair? x) (eqv? (car x) 'memory)
                                (let ((type (case (cadr x)
                                              ((1) "u8")
                                              ((2) "u16")
                                              ((4) "u32")
                                              (else "u64"))))
                                  (string-append "*(" type "*)(" (Expr (caddr x)) ")")))))
  (define (Expr x)
    (define (Subexpr x) (if (pair? x) (string-append "(" (Expr x) ")") (Expr x)))
    (cond ((Location x))
          ((integer? x) (string-append (number->string x) u64-suffix))
          (else (let ((op (car x)) (a (Subexpr (cadr x))) (b (Subexpr (caddr x))))
                  (define (infix a op b) (string-append a " " op " " b))
                  (case op
                    ((+ - *)   (string-append a " " (symbol->string op) " " b))
                    ((and)     (string-append a " & " b))
                    ((ior)     (string-append a " | " b))
                    ((xor)     (string-append a " ^ " b))
                    ((asl lsl) (string-append a " << " b))
                    ((lsr)     (string-append a " >> " b))
                    ((asr)     (string-append "((s64)" a ") < 0 ? "
                                              "~(~" a " >> " b ")" " : " a " >> " b))
                    (else (mistake "invalid binary operator" x)))))))
  (let loop ((S P))
    (if (Label? S)
        (emit (string-append S ":\n"))
        (apply (case (car S)
                 ((set!) (lambda (lhs rhs)
                           (emit (if (Label? rhs)
                                     (string-append (Location lhs) " = (u64*)&&" rhs ";\n")
                                     (string-append (Location lhs) " = " (Expr rhs) ";\n")))))
                 ((jump) (lambda (x) (emit (if (Label? x)
                                               (string-append "goto " x ";\n")
                                               (string-append "goto *(void*)(" (Expr x) ");\n")))))
                 ((begin) (lambda S* (for-each loop S*)))
                 (else (mistake "not a Statement" S)))
               (cdr S)))))
