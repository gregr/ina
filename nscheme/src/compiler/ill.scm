;;;;;;;;;;;
;;; ILL ;;;
;;;;;;;;;;;
;; Program      ::= (letrec ((Label (case-lambda (Param* Block) ...)) ...) Block)
;; Block        ::= (fresh (Var ...) Values)
;; Values       ::= (if Condition Values Values)
;;                | (begin Effect ... Values)
;;                | (case-set!-values Values (LHS* Values) ...)
;;                | General
;;                | (Binary-op2 Value Value)
;;                | (addc Value Value Value)
;;                | (subc Value Value Value)
;;                | (values Value ...)
;; Value        ::= (if Condition Value Value)
;;                | (begin Effect ... Value)
;;                | (case-set!-values Values (LHS* Value) ...)
;;                | General
;; Condition    ::= (if Condition Condition Condition)
;;                | (begin Effect ... Condition)
;;                | (case-set!-values Values (LHS* Condition) ...)
;;                | Boolean
;; Effect       ::= (if Condition Effect Effect)
;;                | (begin Effect ... Effect)
;;                | (case-set!-values Values (LHS* Effect) ...)
;;                | ()
;;                | (set!-values LHS* Values)
;;                | (set! Location Value)
;;                | (set! Var1 (atomic-cas Memory8 Var1 Value))
;;                | Call
;;                | (set!-values (LHS ...) Foreign-call)
;;                | (set! Location Foreign-call)
;;                | Foreign-call
;; Foreign-call ::= (foreign-call Value Value ...)
;; General      ::= Simple | Call | (malloc Value)
;; Call         ::= (call Value Value ...)
;;                | (apply Value Value ...)
;;                | (apply/values Value Values)
;; Simple       ::= SU64 | Label | Location | (Binary-op Value Value) | Boolean
;; Boolean      ::= #f | #t | (Compare-op Value Value)
;; Binary-op    ::= + | - | * | and | ior | xor | asl | asr | lsl | lsr
;; Binary-op2   ::= +/carry | +/over | -/carry | -/over | */over | u128*
;; Compare-op   ::= and | nand | = | =/= | < | <= | > | >= | u< | u<= | u> | u>=
;; LHS*         ::= LHS | (LHS ...) | (LHS LHS ... . LHS)
;; LHS          ::= Location | #f
;; Param*       ::= Param | (Param ...) | (Param Param ... . Param)
;; Param        ::= Var | #f
;; Location     ::= Var | Memory
;; Memory       ::= Memory8 | Memory4 | Memory2 | Memory1
;; Memory4      ::= #(mloc 4 Value Value Value IShift)
;; Memory8      ::= #(mloc 8 Value Value Value IShift)
;; Memory2      ::= #(mloc 2 Value Value Value IShift)
;; Memory1      ::= #(mloc 1 Value Value Value IShift)
;; IShift       ::= 0 | 1 | 2 | 3
;; Var          ::= <symbol>
;; SU64         ::= <signed or unsigned 64-bit integer>
;; Label        ::= <string>
(splicing-local
  ((define Label? string?)
   (define Var? symbol?))

  (define (ILL-validate P)
    (define cmpop '(and nand = =/= < <= > >= u< u<= u> u>=))
    (define binop '(+ - * and ior xor asl asr lsl lsr))
    (define binop2 '(+/carry +/over -/carry -/over */over u128*))
    (define (SU64? x) (and (integer? x) (or (<= s64-min x u64-max)
                                            (mistake "not a signed or unsigned 64-bit integer" x))))
    (define (Label?!/ctx ctx x) (or (Label? x) (mistake "not a Label" x ctx)))
    (define (Var?!/ctx ctx x) (or (Var? x) (mistake "not a Var" x ctx)))
    (define (Var*?!/ctx ctx x)
      (unless (list? x) (mistake "not a list" x ctx))
      (andmap (lambda (x) (Var?!/ctx ctx x)) x))
    (define (Memory? x)
      (and (mloc? x) (or (memv (mloc-width x) '(8 4 2 1)) (mistake "invalid mloc width" x))
           (let ((b (mloc-base x)) (d (mloc-disp x)) (i (mloc-index x)) (s (mloc-shift x)))
             (and (Value?!/ctx x b) (Value?!/ctx x d) (Value?!/ctx x i)
                  (or (memv s '(0 1 2 3)) (mistake "invalid mloc shift" x))))))
    (define (Memory?! x) (or (Memory? x) (mistake "not a memory location" x)))
    (define (Memory8?! x) (or (and (Memory?! x) (eqv? (mloc-width x) 8))
                              (mistake "memory width is not 8" x)))
    (define (Location? x) (or (Var? x) (Memory? x)))
    (define (Location?!/ctx ctx x) (unless (Location? x) (mistake "not a location" x ctx)))
    (define (Param? x) (or (Var? x) (not x)))
    (define (Param*? x) (or (Param? x) (null? x) (and (pair? x) (Param? (car x)) (Param*? (cdr x)))))
    (define (Param*?!/ctx ctx x) (or (Param*? x) (mistake "not a parameter list" x ctx)))
    (define (LHS? x) (or (Location? x) (not x)))
    (define (LHS*? x) (or (LHS? x) (null? x) (and (pair? x) (LHS? (car x)) (LHS*? (cdr x)))))
    (define (LHS*?!/ctx ctx x) (or (LHS*? x) (mistake "not a left-hand-side list" x ctx)))
    (define (operation? x tag handle)
      (and (pair? x) (if (procedure? tag) (tag (car x)) (eqv? (car x) tag))
           (or (list? (cdr x)) (mistake "not a list" x)) (apply handle (cdr x))))
    (define (Compare-op? x)
      (operation? x (lambda (t) (memv t cmpop))
                  (case-lambda ((a b) (and (Value?!/ctx x a) (Value?!/ctx x b)))
                               (_ (mistake "operator arity mismatch" x)))))
    (define (Binary-op2? x)
      (operation? x (lambda (t) (memv t binop2))
                  (case-lambda ((a b) (and (Value?!/ctx x a) (Value?!/ctx x b)))
                               (_ (mistake "operator arity mismatch" x)))))
    (define (Binary-op? x)
      (operation? x (lambda (t) (memv t binop))
                  (case-lambda ((a b) (and (Value?!/ctx x a) (Value?!/ctx x b)))
                               (_ (mistake "operator arity mismatch" x)))))
    (define (Boolean? x) (case x ((#f #t) #t) (else (Compare-op? x))))
    (define (Simple? x) (or (SU64? x) (Label? x) (Location? x) (Binary-op? x) (Boolean? x)))
    (define (Call? x)
      (or (operation? x (lambda (t) (memv t '(call apply)))
                      (case-lambda
                        ((rator . rand*) (Value?!/ctx x rator)
                                         (andmap (lambda (rand) (Value?!/ctx x rand)) rand*))
                        (_ (mistake "operator arity mismatch" x))))
          (operation? x 'apply/values
                      (case-lambda
                        ((rator vrand) (Value?!/ctx x rator) (Values?!/ctx x vrand))
                        (_ (mistake "operator arity mismatch" x))))))
    (define (General? x)
      (or (Simple? x) (Call? x)
          (operation? x 'malloc (case-lambda
                                  ((size) (Value?!/ctx x size))
                                  (_ (mistake "operator arity mismatch" x))))))
    (define (Foreign-call? x)
      (operation? x 'foreign-call
                  (case-lambda
                    ((rator . rand*) (Value?!/ctx x rator)
                                     (andmap (lambda (rand) (Value?!/ctx x rand)) rand*))
                    (_ (mistake "operator arity mismatch" x)))))
    (define (Compound?/Expr?!/ctx Expr?!/ctx x)
      (or (operation? x 'if
                      (case-lambda
                        ((c t f) (Condition?!/ctx x c) (Expr?!/ctx x t) (Expr?!/ctx x f))
                        (_ (mistake "malformed if" x))))
          (operation? x 'begin
                      (lambda x* (let ((rx* (reverse x*)))
                                   (andmap (lambda (e) (Effect?!/ctx x* e)) (reverse (cdr rx*)))
                                   (Expr?!/ctx x* (car rx*)))))
          (operation? x 'case-set!-values
                      (case-lambda
                        ((scrutinee . clause*)
                         (Value?!/ctx x scrutinee)
                         (andmap (lambda (c)
                                   (unless (list? c) (mistake "not a list" c x))
                                   (apply (case-lambda
                                            ((p* e) (LHS*?!/ctx x p*) (Expr?!/ctx x e))
                                            (_ (mistake "malformed case-set!-values clause" c x)))
                                          c))
                                 clause*))
                        (_ (mistake "malformed case-set!-values" x))))))
    (define (CAS?/lhs lhs x)
      (operation? x 'atomic-cas
                  (case-lambda
                    ((loc expected new)
                     (unless (Var? lhs) (mistake "atomic-cas is not assigned to a variable" lhs x))
                     (unless (eqv? expected lhs)
                       (mistake "atomic-cas expected value is not reassigned" lhs expected x))
                     (and (Memory8?! loc) (Value?!/ctx x new)))
                    (_ (mistake "operator arity mismatch" x)))))
    (define (Effect? x)
      (or (operation? x 'set!-values
                      (case-lambda
                        ((p* rhs)
                         (LHS*?!/ctx x p*)
                         (or (and (Foreign-call? rhs)
                                  (or (not (LHS? p*))
                                      (mistake "foreign-call cannot have variadic return" x)))
                             (Values?!/ctx x rhs)))
                        (_ (mistake "set!-values arity mismatch" x))))
          (operation? x 'set!
                      (case-lambda
                        ((lhs rhs) (Location?!/ctx x lhs)
                                   (or (Foreign-call? rhs) (Value?!/ctx x rhs) (CAS?/lhs lhs rhs)))
                        (_ (mistake "set! arity mismatch" x))))
          (Call? x) (Foreign-call? x) (null? x)
          (Compound?/Expr?!/ctx Effect?!/ctx x)))
    (define (Effect?!/ctx ctx x) (or (Effect? x) (mistake "not an Effect" x ctx)))
    (define (Condition? x) (or (Boolean? x) (Compound?/Expr?!/ctx Condition?!/ctx x)))
    (define (Condition?!/ctx ctx x) (or (Condition? x) (mistake "not a Condition" x ctx)))
    (define (Value? x) (or (General? x) (Compound?/Expr?!/ctx Value?!/ctx x)))
    (define (Value?!/ctx ctx x) (or (Value? x) (mistake "not a Value" x ctx)))
    (define (Values? x)
      (or (General? x) (Binary-op2? x) (Compound?/Expr?!/ctx Values?!/ctx x)
          (operation? x (lambda (t) (memv t '(addc subc)))
                      (case-lambda
                        ((a b c) (Value?!/ctx x a) (Value?!/ctx x b) (Value?!/ctx x c))
                        (_ (mistake "operator arity mismatch" x))))
          (operation? x 'values (lambda v* (andmap (lambda (v) (Value?!/ctx x v)) v*)))))
    (define (Values?!/ctx ctx x) (or (Values? x) (mistake "not a Values" x ctx)))
    (define (Block?!/ctx ctx x)
      (or (operation? x 'fresh (case-lambda ((v* e) (Var*?!/ctx x v*) (Values?!/ctx x e))
                                            (_ (mistake "malformed fresh" x))))
          (mistake "not a Block" x ctx)))
    (define (Letrec?! x)
      (define (Lambda?! lam)
        (define (Clause?! c)
          (or (and (list? c) (apply (case-lambda
                                      ((p* b) (Param*?!/ctx lam p*) (Block?!/ctx lam b))
                                      (_ (mistake "malformed case-lambda clause" lam x)))
                                    c))
              (mistake "not a list" c)))
        (or (operation? lam 'case-lambda (lambda clause* (andmap Clause?! clause*)))
            (mistake "not a case-lambda" lam x)))
      (or (operation? x 'letrec
                      (case-lambda
                        ((ll* b) (unless (list? ll*) (mistake "not a list" ll* x))
                                 (andmap (lambda (ll)
                                           (unless (list? ll) (mistake "not a list" ll x))
                                           (apply (case-lambda
                                                    ((label lam) (Label?!/ctx x label) (Lambda?! lam))
                                                    (_ (mistake "not a binding pair" ll x)))
                                                  ll))
                                         ll*)
                                 (Block?!/ctx x b))
                        (_ (mistake "malformed letrec" x))))
          (mistake "not a letrec" x)))
    (Letrec?! P)))
