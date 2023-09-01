;;; NOTE: this evaluator is only intended for testing until the compiler is finished.
(splicing-local
  ((define cenv.empty '())
   (define renv.empty '())
   (define (cenv-ref cenv.0 addr)
     (let loop ((j 0) (cenv.1 cenv.0))
       (when (null? cenv.1) (error "unbound or out-of-phase reference" addr))
       (let ((rec?      (caar cenv.1))
             (variadic? (cadar cenv.1))
             (addrs     (cddar cenv.1))
             (cenv      (cdr cenv.1)))
         (let find ((i 0) (a* addrs))
           (if (null? a*)
               (loop (+ j 1) cenv)
               (let ((a (car a*)) (a* (cdr a*)))
                 (if (eq? a addr)
                     (if rec?
                         (if variadic?
                             (error "recursive frame cannot be variadic" cenv.1)
                             (lambda (renv) (unbox (list-ref (list-ref renv j) i))))
                         (if (and variadic? (null? a*))
                             (lambda (renv) (list-tail (list-ref renv j) i))
                             (lambda (renv) (list-ref  (list-ref renv j) i))))
                     (find (+ i 1) a*))))))))
   (define (cenv-extend      cenv param) (cenv-extend/rec? cenv param #f))
   (define (cenv-extend-rec  cenv param) (cenv-extend/rec? cenv param #t))
   (define (cenv-extend/rec? cenv param.0 rec?)
     (cons (cons rec? (let loop ((param param.0) (addrs '()))
                        (cond ((null? param) (cons #f param.0))
                              ((pair? param) (loop (cdr param) (cons (car param) addrs)))
                              (else          (cons #t (reverse (cons param addrs)))))))
           cenv))
   (define (param-arity param)
     (let loop ((param param) (arity 0))
       (cond ((null? param) arity)
             ((pair? param) (loop (cdr param) (+ arity 1)))
             (else          (- (+ arity 1))))))
   (define (make-renv-extend arity count)
     (and (if (< arity 0)
              (<= (- (+ arity 1)) count)
              (= arity count))
          (lambda (renv args) (cons args renv))))

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
          (let ((k (foldr (lambda (param body k)
                            (let* ((cenv  (cenv-extend cenv param))
                                   (body  (loop/env body cenv))
                                   (arity (param-arity param)))
                              (lambda (renv count args)
                                (let ((renv-extend (make-renv-extend arity count)))
                                  (if renv-extend
                                      (body (renv-extend renv args))
                                      (k renv count args))))))
                          (lambda (renv count args)
                            (error "arity mismatch" 'given count 'expected (E:case-lambda-param* E)))
                          (E:case-lambda-param* E) (E:case-lambda-body* E))))
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
         ((E:prim? E)
          (let ((p (case (E:prim-name E)
                     ((current-control-context) current-control-context)
                     ((make-control-context) make-control-context)
                     ((control-context-register) control-context-register)
                     ((set-control-context-register!) set-control-context-register!)
                     ((panic) panic) ((set-panic-handler!) set-panic-handler!)
                     ((yield) yield) ((set-yield-handler!) set-yield-handler!)
                     ((set-timer) set-timer)
                     ((enable-interrupts) enable-interrupts)
                     ((disable-interrupts) disable-interrupts)
                     ((procedure-metadata) procedure-metadata) ((record?) record?) ((record) record)
                     ((record-type-descriptor) record-type-descriptor) ((record-ref) record-ref)
                     ((string->bytevector) string->bytevector)
                     ((bytevector->string) bytevector->string)
                     ((apply) apply) ((values) values)
                     ((eq?) eq?) ((eqv?) eqv?) ((null?) null?) ((boolean?) boolean?)
                     ((procedure?) procedure?) ((symbol?) symbol?) ((string?) string?)
                     ((rational?) rational?) ((integer?) integer?)
                     ((pair?) pair?) ((vector?) vector?) ((mvector?) mvector?)
                     ((bytevector?) bytevector?) ((mbytevector?) mbytevector?)
                     ((string->symbol) string->symbol) ((symbol->string) symbol->string)
                     ((cons) cons) ((car) car) ((cdr) cdr)
                     ((vector) vector) ((vector-length) vector-length) ((vector-ref) vector-ref)
                     ((make-mvector) make-mvector) ((mvector->vector) mvector->vector)
                     ((mvector-length) mvector-length)
                     ((mvector-ref) mvector-ref) ((mvector-set!) mvector-set!)
                     ((bytevector) bytevector) ((bytevector-length) bytevector-length)
                     ((bytevector-u8-ref) bytevector-u8-ref)
                     ((make-mbytevector) make-mbytevector)
                     ((mbytevector->bytevector) mbytevector->bytevector)
                     ((mbytevector-length) mbytevector-length)
                     ((mbytevector-u8-ref) mbytevector-u8-ref)
                     ((mbytevector-u8-set!) mbytevector-u8-set!)
                     ((bitwise-arithmetic-shift-left) bitwise-arithmetic-shift-left)
                     ((bitwise-arithmetic-shift-right) bitwise-arithmetic-shift-right)
                     ((bitwise-not) bitwise-not) ((bitwise-and) bitwise-and)
                     ((bitwise-ior) bitwise-ior) ((bitwise-xor) bitwise-xor)
                     ((bitwise-length) bitwise-length) ((integer-floor-divmod) integer-floor-divmod)
                     ((numerator) numerator) ((denominator) denominator)
                     ((=) =) ((<=) <=) ((>=) >=) ((<) <) ((>) >) ((+) +) ((-) -) ((*) *) ((/) /)
                     (=> (lambda (name) (error "not a primitive" name))))))
            (lambda (renv) p)))
         (else (error "not an expression" E))))))
  (define (E-eval E) ((E-stage E cenv.empty) renv.empty)))

(define (E-pretty E)
  (let loop ((E E))
    (cond
      ((E:annotated?   E) (loop (E:annotated-E E)))
      ((E:prim?        E) (list 'prim  (E:prim-name   E)))
      ((E:quote?       E) (list 'quote (E:quote-value E)))
      ((E:ref?         E) (list 'ref   (E:ref-address E)))
      ((E:if?          E) (list 'if (loop (E:if-condition E))
                                (loop (E:if-consequent E))
                                (loop (E:if-alternative E))))
      ((E:call?        E) (cons* 'call (loop (E:call-operator E)) (map loop (E:call-operand* E))))
      ((E:case-lambda? E) (cons 'case-lambda (map list (E:case-lambda-param* E)
                                                  (map loop (E:case-lambda-body* E)))))
      ((E:letrec?      E) (list 'letrec (map list (E:letrec-binding-left* E)
                                             (map loop (E:letrec-binding-right* E)))
                                (loop (E:letrec-body E))))
      (else               (error "not an expression" E)))))
