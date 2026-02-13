;;;;;;;;;;;;;;;;;
;;; LLL for C ;;;
;;;;;;;;;;;;;;;;;
(define LLL-C-options-gcc "-fno-strict-aliasing")
(define LLL-C-prelude
  ##eos"typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef signed char s8;
typedef signed short s16;
typedef signed int s32;
typedef signed long long s64;
typedef unsigned __int128 u128;
typedef struct { u64 low, high; } u64pair;
static inline u64pair LLL_umul128(u64 a, u64 b) {
 u128 full = ((u128)a) * ((u128)b);
 u64pair p;
 p.low = full;
 p.high = full >> 64;
 return p; }
static inline u64 LLL_atomic_cas(u64* loc, u64 expected, u64 new) {
 __atomic_compare_exchange_n(loc, &expected, new, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
 return expected; }
"eos##)
(define LLL-C-prelude-local "u64pair LLL_u128; u64 LLL_flag_carry, LLL_flag_over;")

(define (LLL-validate-C P)
  (define Label? string?)
  (LLL-validate P)
  (let loop ((x P))
    (cond ((pair? x) (loop (car x)) (loop (cdr x)))
          ((mloc? x) (when (Label? (mloc-disp x))
                       (mistake "LLL C does not support mloc with label displacement" x)))
          (else (values)))))

(define (LLL-emit-C P)
  (define u64-suffix "ull")
  (define emit (let ((out (current-output-port))) (lambda (line) (display line out))))
  (define Label? string?)
  (define (width->type w) (case w ((1) "u8") ((2) "u16") ((4) "u32") (else "u64")))
  (define (width-cast x) (let ((w (if (mloc? x) (mloc-width x) 8)))
                           (if (eqv? w 8) "" (string-append "(" (width->type w) ")"))))
  (define (Address x)
    (define (addend x) (if (eqv? x 0) "" (Subexpr x)))
    (define (shifted x s) (if (or (= s 0) (eqv? x ""))
                              x
                              (string-append "(" x "<<" (number->string s) ")")))
    (let ((b (mloc-base x)) (d (mloc-disp x)) (i (mloc-index x)) (s (mloc-shift x)))
      (string-join* " + " (filter (lambda (x) (not (eqv? x "")))
                                  (list (addend b) (addend d) (shifted (addend i) s))))))
  (define (Ref x) (string-append "(" (width->type (mloc-width x)) "*)(" (Address x) ")"))
  (define (Location x)
    (if (symbol? x) (symbol->string x) (and (mloc? x) (string-append "*" (Ref x)))))
  (define (SU64 x) (and (integer? x) (string-append (number->string (s64 x)) u64-suffix)))
  (define (cc x) (and (eqv? (car x) 'cc) (case (cadr x)
                                           ((carry)  "LLL_flag_carry")
                                           ((ncarry) "!LLL_flag_carry")
                                           ((over)   "LLL_flag_overflow")
                                           ((nover)  "!LLL_flag_overflow")
                                           (else (mistake "not a condition code" (cadr x))))))
  (define (If x) (and (eqv? (car x) 'if)
                      (apply (lambda (c t f) (string-append "(" (Condition c) ") ? "
                                                            (Subexpr t) " : " (Subexpr f)))
                             (cdr x))))
  (define (Binary-op x)
    (let* ((op (car x)) (xa (cadr x)) (xb (caddr x)) (a (Subexpr xa)) (b (Subexpr xb)))
      (case op
        ((+ - *)   (string-append a " " (symbol->string op) " " b))
        ((and)     (string-append a " & " b))
        ((ior)     (string-append a " | " b))
        ((xor)     (cond ((eqv? xa -1) (string-append "~" b))
                         ((eqv? xb -1) (string-append "~" a))
                         (else (string-append a " ^ " b))))
        ((asl lsl) (string-append a " << " b))
        ((lsr)     (string-append a " >> " b))
        ((asr)     (string-append "((s64)" a ") < 0 ? ~(~" a " >> " b ") : " a " >> " b))
        ((and)     (string-append "(" a " & " b ") != 0"))
        ((nand)    (string-append "(" a " & " b ") == 0"))
        ((=)       (cond ((eqv? xa 0) (string-append "!" b))
                         ((eqv? xb 0) (string-append "!" a))
                         (else (string-append a " == " b))))
        ((=/=)     (cond ((eqv? xa 0) (string-append "!!" b))
                         ((eqv? xb 0) (string-append "!!" a))
                         (else (string-append a " != " b))))
        ((<)       (string-append "(s64)" a " < (s64)" b))
        ((<=)      (string-append "(s64)" a " <= (s64)" b))
        ((>)       (string-append "(s64)" a " > (s64)" b))
        ((>=)      (string-append "(s64)" a " >= (s64)" b))
        ((u<)      (string-append a " < " b))
        ((u<=)     (string-append a " <= " b))
        ((u>)      (string-append a " > " b))
        ((u>=)     (string-append a " >= " b))
        (else (mistake "invalid binary operator" x)))))
  (define (Subexpr x) (if (pair? x) (string-append "(" (Expr x) ")") (Expr x)))
  (define (Expr x) (or (Location x) (SU64 x) (cc x) (If x) (Binary-op x)))
  (define (Condition x) (if (and (pair? x) (eqv? (car x) '=/=))
                            (let ((xa (cadr x)) (xb (caddr x)))
                              (cond ((eqv? xa 0) (Expr xb))
                                    ((eqv? xb 0) (Expr xa))
                                    (else (Expr x))))
                            (Expr x)))
  (define (Call x) (let ((rator (car x)) (rand* (cdr x)))
                     (string-append (if (Label? rator) rator (Subexpr rator))
                                    "(" (string-join* "," (map Expr rand*)) ")")))
  (define (Assign lhs x)
    (define (simple rhs) (string-append " " (Location lhs) " = " (width-cast lhs) rhs ";\n"))
    (define (overflow f op identity x)
      (define (clear-assign x) (string-append " LLL_flag_" f " = 0;\n"
                                              (if (equal? lhs x) "" (simple (Expr x)))))
      (let ((a (car x)) (b (cadr x)))
        (cond ((eqv? a identity) (clear-assign b))
              ((eqv? b identity) (clear-assign a))
              (else (string-append " LLL_flag_" f " = __builtin_" op "ll_overflow("
                                   (Expr a) "," (Expr b) ",&" (Location lhs) ");\n")))))
    (cond ((Label? x) (simple (string-append "(u64*)&&" x)))
          ((or (Location x) (SU64 x) (cc x) (If x)) => simple)
          ((case (car x)
             ((call) (Call (cdr x)))
             ((lea) (Address (cadr x)))
             ((addc subc) (let ((a (Expr (cadr x))) (b (Expr (caddr x))))
                            (string-append "__builtin_" (if (eqv? (car x) 'addc) "add" "sub")
                                           "cll(" a "," b ",LLL_flag_carry,&LLL_flag_carry);")))
             ((atomic-cas)
              (let* ((x (cdr x)) (loc (car x)) (x (cdr x)) (expected (car x)) (new (cadr x)))
                (string-append "LLL_atomic_cas(" (Ref loc) "," (Expr expected) "," (Expr new) ")")))
             (else #f)) => simple)
          (else (case (car x)
                  ((+/carry) (overflow "carry" "uadd" 0 (cdr x)))
                  ((-/carry) (overflow "carry" "usub" 0 (cdr x)))
                  ((+/over) (overflow "overflow" "sadd" 0 (cdr x)))
                  ((-/over) (overflow "overflow" "ssub" 0 (cdr x)))
                  ((*/over) (overflow "overflow" "smul" 1 (cdr x)))
                  (else (simple (Binary-op x)))))))
  (define (Assign2 l1 l2 x)
    (case (car x)
      ((u128*) (apply (lambda (a b)
                        (string-append " LLL_u128 = LLL_umul128(" (Expr a) "," (Expr b) ");\n"
                                       " " (Location l1) " = LLL_u128.high;\n"
                                       " " (Location l2) " = LLL_u128.low;\n"))
                      (cdr x)))
      (else (mistake "invalid Expr128" x))))
  (let loop ((S P))
    (if (Label? S)
        (emit (string-append S ":\n"))
        (apply (case (car S)
                 ((set!) (lambda (lhs rhs) (emit (Assign lhs rhs))))
                 ((set2!) (lambda (lhs1 lhs2 rhs) (emit (Assign2 lhs1 lhs2 rhs))))
                 ((jump-if) (lambda (x label)
                              (emit (string-append " if (" (Condition x) ") goto " label ";\n"))))
                 ((jump) (lambda (x) (emit (if (Label? x)
                                               (string-append " goto " x ";\n")
                                               (string-append " goto *(void*)(" (Expr x) ");\n")))))
                 ((call) (lambda _ (emit (string-append " " (Call (cdr S)) ";\n"))))
                 ((begin) (lambda S* (for-each loop S*)))
                 (else (mistake "not a Statement" S)))
               (cdr S)))))
