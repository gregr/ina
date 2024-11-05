(define (make-address name note*) (vector (mvector) name note*))
(define (address-name       addr) (vector-ref addr 1))
(define (address-note*      addr) (vector-ref addr 2))
(define (address=?           a b) (or (eq? a b) (eq? (vector-ref a 0) (vector-ref b 0))))
(define (address->local-gensym/default name.default)
  (let ((gensym (make-local-gensym)))
    (lambda (addr)
      (let ((name (address-name addr)))
        (gensym (symbol->string (if name name name.default)))))))

;; TODO: support for lower-level language integration:
;; - E:unchecked-call, E:let with type info, E:case-lambda with type info, etc.
;; TODO: we may want to return to the embedded annotation model for simpler pattern matching.
(define (E:annotated    E note*)        (vector 'E:annotated    E note*))
(define (E:quote        v)              (vector 'E:quote        v))
(define (E:ref          address)        (vector 'E:ref          address))
(define (E:if           c t f)          (vector 'E:if           c t f))
(define (E:call         rator rand*)    (vector 'E:call         rator rand*))
(define (E:apply/values rator vrand)    (vector 'E:apply/values rator vrand))
(define (E:case-lambda  param*~* body*) (vector 'E:case-lambda  param*~* body*))
(define (E:letrec       lhs* rhs* body) (vector 'E:letrec       lhs* rhs* body))

(define (E-tag                   E)     (vector-ref E 0))
(define (E-tagged?               E tag) (eq? (E-tag E) tag))
(define (E:annotated?            E)     (E-tagged? E 'E:annotated))
(define (E:quote?                E)     (E-tagged? E 'E:quote))
(define (E:ref?                  E)     (E-tagged? E 'E:ref))
(define (E:if?                   E)     (E-tagged? E 'E:if))
(define (E:call?                 E)     (E-tagged? E 'E:call))
(define (E:apply/values?         E)     (E-tagged? E 'E:apply/values))
(define (E:case-lambda?          E)     (E-tagged? E 'E:case-lambda))
(define (E:letrec?               E)     (E-tagged? E 'E:letrec))
(define (E:annotated-E           E)     (vector-ref E 1))
(define (E:annotated-note*       E)     (vector-ref E 2))
(define (E:quote-value           E)     (vector-ref E 1))
(define (E:ref-address           E)     (vector-ref E 1))
(define (E:if-condition          E)     (vector-ref E 1))
(define (E:if-consequent         E)     (vector-ref E 2))
(define (E:if-alternative        E)     (vector-ref E 3))
(define (E:call-operator         E)     (vector-ref E 1))
(define (E:call-operand*         E)     (vector-ref E 2))
(define (E:apply/values-operator E)     (vector-ref E 1))
(define (E:apply/values-operand  E)     (vector-ref E 2))
(define (E:case-lambda-param*~*  E)     (vector-ref E 1))
(define (E:case-lambda-body*     E)     (vector-ref E 2))
(define (E:letrec-binding-left*  E)     (vector-ref E 1))
(define (E:letrec-binding-right* E)     (vector-ref E 2))
(define (E:letrec-body           E)     (vector-ref E 3))

(define (E-note* E) (if (E:annotated? E) (E:annotated-note* E) '()))
(define (E-note*-set E note*)
  (cond ((eq? (E-note* E) note*) E)
        ((null? note*)           (E:annotated-E E))
        (else                    (E:annotated (if (E:annotated? E) (E:annotated-E E) E) note*))))

(define (E-pretty E)
  (define address-pretty address-name)
  (let loop ((E E))
    (cond
      ((E:annotated?    E) (loop (E:annotated-E E)))
      ((E:quote?        E) (list 'quote (E:quote-value E)))
      ((E:ref?          E) (list 'ref   (address-pretty (E:ref-address E))))
      ((E:if?           E) (list 'if (loop (E:if-condition E))
                                 (loop (E:if-consequent E))
                                 (loop (E:if-alternative E))))
      ((E:call?         E) (cons* 'call (loop (E:call-operator E)) (map loop (E:call-operand* E))))
      ((E:apply/values? E) (list 'apply/values (loop (E:apply/values-operator E))
                                 (loop (E:apply/values-operand E))))
      ((E:case-lambda?  E) (cons 'case-lambda
                                 (map list
                                      (map (lambda (p*~) (improper-list-map address-pretty p*~))
                                           (E:case-lambda-param*~* E))
                                      (map loop (E:case-lambda-body* E)))))
      ((E:letrec?       E) (list 'letrec (map list (map address-pretty (E:letrec-binding-left* E))
                                              (map loop (E:letrec-binding-right* E)))
                                 (loop (E:letrec-body E))))
      (else                (error "not an expression" E)))))

;;; NOTE: this evaluator is only intended for testing until the compiler is finished.
(splicing-local
  ((define (param-arity param)
     (let loop ((param param) (arity 0))
       (cond ((null? param) arity)
             ((pair? param) (loop (cdr param) (+ arity 1)))
             (else          (- (+ arity 1))))))
   (define renv.empty '())
   (define (make-renv-extend arity count)
     (and (if (< arity 0)
              (<= (- (+ arity 1)) count)
              (= arity count))
          (lambda (renv args) (cons args renv))))
   (define cenv.empty '())
   (define (cenv-ref cenv addr)
     (let loop ((j 0) (cenv.1 cenv))
       (when (null? cenv.1) (error "unbound or out-of-phase reference" addr))
       (let ((rec? (caar cenv.1)) (param*~ (cdar cenv.1)) (cenv (cdr cenv.1)))
         (let find ((i 0) (a*~ param*~))
           (cond ((pair? a*~) (let ((a (car a*~)) (a*~ (cdr a*~)))
                                (if (address=? a addr)
                                    (if rec?
                                        (lambda (renv) (unbox (list-ref (list-ref renv j) i)))
                                        (lambda (renv) (list-ref (list-ref renv j) i)))
                                    (find (+ i 1) a*~))))
                 ((and (not (null? a*~)) (address=? a*~ addr))
                  (when rec? (error "recursive frame cannot be variadic" cenv.1))
                  (lambda (renv) (list-tail (list-ref renv j) i)))
                 (else (loop (+ j 1) cenv)))))))
   (define (cenv-extend      cenv param*~)      (cenv-extend/rec? cenv param*~ #f))
   (define (cenv-extend-rec  cenv param*~)      (cenv-extend/rec? cenv param*~ #t))
   (define (cenv-extend/rec? cenv param*~ rec?) (cons (cons rec? param*~) cenv))
   (define (E-stage E cenv)
     (let loop/env ((E E) (cenv cenv))
       (define (loop E) (loop/env E cenv))
       (cond
         ((E:annotated?    E) (loop (E:annotated-E E)))
         ((E:quote?        E) (let ((x (E:quote-value E))) (lambda (renv) x)))
         ((E:ref?          E) (cenv-ref cenv (E:ref-address E)))
         ((E:if?           E) (let ((c (loop (E:if-condition E)))
                                    (t (loop (E:if-consequent E)))
                                    (f (loop (E:if-alternative E))))
                                (lambda (renv) (if (c renv) (t renv) (f renv)))))
         ((E:call?         E) (let ((rator (loop (E:call-operator E)))
                                    (rand* (map loop (E:call-operand* E))))
                                (lambda (renv)
                                  (apply (rator renv) (map (lambda (rand) (rand renv)) rand*)))))
         ((E:apply/values? E) (let ((rator (loop (E:apply/values-operator E)))
                                    (vrand (loop (E:apply/values-operand E))))
                                (lambda (renv) (apply/values (rator renv) (vrand renv)))))
         ((E:case-lambda?  E)
          (let ((k (fold-right
                     (lambda (k param body)
                       (let* ((cenv  (cenv-extend cenv param))
                              (body  (loop/env body cenv))
                              (arity (param-arity param)))
                         (lambda (renv count args)
                           (let ((renv-extend (make-renv-extend arity count)))
                             (if renv-extend
                                 (body (renv-extend renv args))
                                 (k renv count args))))))
                     (lambda (renv count args)
                       (error "arity mismatch" 'given count 'expected (E:case-lambda-param*~* E)))
                     (E:case-lambda-param*~* E) (E:case-lambda-body* E))))
            (lambda (renv) (lambda args (k renv (length args) args)))))
         ((E:letrec? E)
          (let* ((param       (E:letrec-binding-left* E))
                 (arity       (param-arity param))
                 (renv-extend (make-renv-extend arity (length param)))
                 (body        (loop/env (E:letrec-body E) (cenv-extend cenv param)))
                 (cenv        (cenv-extend-rec cenv param))
                 (arg*        (map (lambda (rhs) (loop/env rhs cenv)) (E:letrec-binding-right* E))))
            (lambda (renv) (let ((loc* (map (lambda (_) (box #t)) arg*)))
                             (let ((renv (renv-extend renv loc*)))
                               (for-each (lambda (loc arg) (set-box! loc (arg renv))) loc* arg*))
                             (body (renv-extend renv (map unbox loc*)))))))
         (else (error "not an expression" E))))))
  (define (E-eval E) ((E-stage E cenv.empty) renv.empty)))
