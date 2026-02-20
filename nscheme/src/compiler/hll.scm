;;;;;;;;;;;
;;; HLL ;;;
;;;;;;;;;;;
;; Expr   ::= Var
;;          | Primop
;;          | (quote <value>)  ; upstream is responsible for any complex literal simplification
;;          | (if Expr Expr Expr)
;;          | (call Expr Expr ...)
;;          | (apply/values Expr Expr)
;;          | (case-lambda (Param* Expr) ...)
;;          | (letrec ((Param Expr) ...) Expr)
;;          | (let ((Param Expr) ...) Expr)
;;          | (begin Expr ... Expr)
;;          | (note <value> Expr)
;; Param* ::= Param | (Param ...) | (Param Param ... . Param)
;; Param  ::= Var | #f
;; Var    ::= <uvar>  ; each case-lambda, let, or letrec-bound variable must be unique
;; Primop ::= <primop>
(define (HLL-validate P)
  (define (list?! x) (or (list? x) (mistake "not a list" x)))
  (define (apply?! proc x) (and (list?! x) (apply proc x)))
  (define (operation? x tag handle . tag&handle*) (operation?* x tag handle tag&handle*))
  (define (operation?* x tag handle t&h*)
    (and (pair? x)
         (let ((key (car x)))
           (let loop ((tag tag) (handle handle) (t&h* t&h*))
             (if (if (procedure? tag) (tag (car x)) (eqv? key tag))
                 (apply?! handle (cdr x))
                 (and (pair? t&h*) (pair? (cdr t&h*))
                      (loop (car t&h*) (cadr t&h*) (cddr t&h*))))))))
  (define (Param? x) (or (not x) (uvar? x)))
  (define (Param?!/ctx ctx x) (or (Param? x) (mistake "not a Param" x ctx)))
  (define (Param*?!/ctx ctx x)
    (or (Param? x) (null? x)
        (and (pair? x) (Param?!/ctx ctx (car x)) (Param*?!/ctx ctx (cdr x)))
        (mistake "not a parameter list" x ctx)))
  (define (Expr?!/ctx ctx x)
    (define Let-rand*?!
      (case-lambda
        ((p&e* body)
         (and (list?! p&e*)
              (andmap (lambda (p&e)
                        (apply?! (case-lambda
                                   ((p e) (and (Param*?!/ctx x p) (Expr?!/ctx x e)))
                                   (_ (mistake "not a case-lambda clause" x ctx)))
                                 p&e))
                      p&e*)
              (Expr?!/ctx x body)))
        (_ (mistake "operator arity mismatch" x ctx))))
    (or (uvar? x) (primop? x)
        (operation?
          x
          'quote        (case-lambda
                          ((val) #t)
                          (_ (mistake "operator arity mismatch" x ctx)))
          'if           (case-lambda
                          ((c t f) (and (Expr?!/ctx x c) (Expr?!/ctx x t) (Expr?!/ctx x f)))
                          (_ (mistake "operator arity mismatch" x ctx)))
          'call         (case-lambda
                          ((rator . rand*) (and (Expr?!/ctx x rator) (Expr*?!/ctx x rand*)))
                          (_ (mistake "operator arity mismatch" x ctx)))
          'apply/values (case-lambda
                          ((rator . rand) (and (Expr?!/ctx x rator) (Expr?!/ctx x rand)))
                          (_ (mistake "operator arity mismatch" x ctx)))
          'case-lambda  (lambda p&b*
                          (andmap (lambda (p&b)
                                    (apply?! (case-lambda
                                               ((p b) (and (Param*?!/ctx x p) (Expr?!/ctx x b)))
                                               (_ (mistake "not a case-lambda clause" x ctx)))
                                             p&b))
                                  p&b*))
          'letrec       Let-rand*?!
          'let          Let-rand*?!
          'begin        (case-lambda
                          ((e . e*) (Expr?!/ctx x e) (Expr*?!/ctx x e*))
                          (_ (mistake "operator arity mismatch" x ctx)))
          'note         (case-lambda
                          ((n e) (Expr?!/ctx x e))
                          (_ (mistake "operator arity mismatch" x ctx))))
        (mistake "not an Expr" x ctx)))
  (define (Expr*?!/ctx ctx x*) (andmap (lambda (x) (Expr?!/ctx ctx x)) x*))
  (Expr?!/ctx #f P))
