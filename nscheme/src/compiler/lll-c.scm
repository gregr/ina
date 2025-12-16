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
  (define (Location x)
    (define (addend x) (if (eqv? x 0) "" (string-append " + " (Subexpr x))))
    (if (symbol? x)
        (symbol->string x)
        (and (mloc? x) (let ((type (case (mloc-width x)
                                     ((1) "u8")
                                     ((2) "u16")
                                     ((4) "u32")
                                     (else "u64"))))
                         (string-append "*(" type "*)(" (Subexpr (mloc-base x))
                                        (addend (mloc-disp x)) (addend (mloc-index x)) ")")))))
  (define (Subexpr x) (if (pair? x) (string-append "(" (Expr x) ")") (Expr x)))
  (define (Expr x)
    (cond ((Location x))
          ((integer? x) (string-append (number->string x) u64-suffix))
          ((and (pair? x) (eqv? (car x) 'call))
           (let ((rator (cadr x)) (rand* (cddr x)))
             (string-append (if (Label? rator) rator (Subexpr rator))
                            "(" (string-join* "," (map Expr rand*)) ")")))
          (else (let ((op (car x)) (a (Subexpr (cadr x))) (b (Subexpr (caddr x))))
                  (case op
                    ((+ - *)   (string-append a " " (symbol->string op) " " b))
                    ((and)     (string-append a " & " b))
                    ((ior)     (string-append a " | " b))
                    ((xor)     (string-append a " ^ " b))
                    ((asl lsl) (string-append a " << " b))
                    ((lsr)     (string-append a " >> " b))
                    ((asr)     (string-append "((s64)" a ") < 0 ? "
                                              "~(~" a " >> " b ")" " : " a " >> " b))
                    ((and)     (string-append "(" a " & " b ") != 0"))
                    ((nand)    (string-append "(" a " & " b ") == 0"))
                    ((=)       (string-append a " == " b))
                    ((=/=)     (string-append a " != " b))
                    ((<)       (string-append "(s64)" a " < (s64)" b))
                    ((<=)      (string-append "(s64)" a " <= (s64)" b))
                    ((>)       (string-append "(s64)" a " > (s64)" b))
                    ((>=)      (string-append "(s64)" a " >= (s64)" b))
                    ((u<)      (string-append a " < " b))
                    ((u<=)     (string-append a " <= " b))
                    ((u>)      (string-append a " > " b))
                    ((u>=)     (string-append a " >= " b))
                    (else (mistake "invalid binary operator" x)))))))
  (let loop ((S P))
    (if (Label? S)
        (emit (string-append S ":\n"))
        (apply (case (car S)
                 ((set!) (lambda (lhs rhs)
                           (emit (if (Label? rhs)
                                     (string-append " " (Location lhs) " = (u64*)&&" rhs ";\n")
                                     (string-append " " (Location lhs) " = " (Expr rhs) ";\n")))))
                 ((jump-if) (lambda (cmp label)
                              (emit (string-append " if (" (Expr cmp) ") goto " label ";\n"))))
                 ((jump) (lambda (x) (emit (if (Label? x)
                                               (string-append " goto " x ";\n")
                                               (string-append " goto *(void*)(" (Expr x) ");\n")))))
                 ((call) (lambda _ (emit (string-append " " (Expr S) ";\n"))))
                 ((begin) (lambda S* (for-each loop S*)))
                 (else (mistake "not a Statement" S)))
               (cdr S)))))
