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
                             (lambda (renv k) (k (unbox (list-ref (list-ref renv j) i)))))
                         (if (and variadic? (null? a*))
                             (lambda (renv k) (k (list-tail (list-ref renv j) i)))
                             (lambda (renv k) (k (list-ref  (list-ref renv j) i)))))
                     (find (+ i 1) a*))))))))
   (define (cenv-extend      cenv param) (cenv-extend/rec? cenv param #f))
   (define (cenv-extend-rec  cenv param) (cenv-extend/rec? cenv param #t))
   (define (cenv-extend/rec? cenv param rec?)
     (cons (cons rec? (let loop ((param param) (addrs '()))
                        (cond ((null? param) (cons #f (reverse addrs)))
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
         ((E:quote?        E) (let ((x (E:quote-value E))) (lambda (renv k) (k x))))
         ((E:ref?          E) (cenv-ref cenv (E:ref-address E)))
         ((E:if?           E) (let ((c (loop (E:if-condition E)))
                                    (t (loop (E:if-consequent E)))
                                    (f (loop (E:if-alternative E))))
                                (lambda (renv k) (c renv (lambda (c) (if c (t renv k) (f renv k)))))))
         ((E:call?         E) (let ((rator (loop (E:call-operator E)))
                                    (rand* (map loop (E:call-operand* E))))
                                (lambda (renv k)
                                  (rator renv (lambda (proc)
                                                (let loop ((rand* rand*) (rarg* '()))
                                                  (if (null? rand*)
                                                      (apply proc k (reverse rarg*))
                                                      ((car rand*) renv
                                                                   (lambda (arg)
                                                                     (loop (cdr rand*)
                                                                           (cons arg rarg*)))))))))))
         ((E:apply/values? E) (let ((rator (loop (E:apply/values-operator E)))
                                    (vrand (loop (E:apply/values-operand E))))
                                (lambda (renv k)
                                  (rator renv (lambda (proc)
                                                (vrand renv (lambda arg* (apply proc k arg*))))))))
         ((E:case-lambda?  E)
          (let ((choose (foldr (lambda (param body choose)
                                 (let* ((cenv  (cenv-extend cenv param))
                                        (body  (loop/env body cenv))
                                        (arity (param-arity param)))
                                   (lambda (renv k count arg*)
                                     (let ((renv-extend (make-renv-extend arity count)))
                                       (if renv-extend
                                           (body (renv-extend renv arg*) k)
                                           (choose renv k count arg*))))))
                               (lambda (renv k count arg*)
                                 (error "arity mismatch" 'given count 'expected
                                        (E:case-lambda-param* E)))
                               (E:case-lambda-param* E) (E:case-lambda-body* E))))
            (lambda (renv k) (k (lambda (k . arg*) (choose renv k (length arg*) arg*))))))
         ((E:letrec? E)
          (let* ((param       (E:letrec-binding-left* E))
                 (arity       (param-arity param))
                 (renv-extend (make-renv-extend arity (length param)))
                 (body        (loop/env (E:letrec-body E) (cenv-extend cenv param)))
                 (cenv        (cenv-extend-rec cenv param))
                 (rhs*        (map (lambda (rhs) (loop/env rhs cenv)) (E:letrec-binding-right* E))))
            (lambda (renv.0 k)
              (let ((loc*.0 (map (lambda (_) (box #t)) rhs*)))
                (let ((renv (renv-extend renv.0 loc*.0)))
                  (let loop ((loc* loc*.0) (rhs* rhs*))
                    (if (null? rhs*)
                        (body (renv-extend renv.0 (map unbox loc*.0)) k)
                        ((car rhs*) renv (lambda (x)
                                           (set-box! (car loc*) x)
                                           (loop (cdr loc*) (cdr rhs*)))))))))))
         ;; TODO: primitives can be run directly, with these exceptions:
         ;; - apply
         ;; - values
         ;; - panic, but we'll ignore this for now
         ;; - the special control primitives, which we'll ignore for now
         ((E:prim? E)
          (let ((p (case (E:prim-name E)
                     ((current-control-context) current-control-context/k)
                     ((make-control-context) make-control-context/k)
                     ((call-with-escape-continuation) call-with-escape-continuation/k)
                     ((call-in-empty-context) call-in-empty-context/k)
                     ((control-context-register) control-context-register/k)
                     ((set-control-context-register!) set-control-context-register!)
                     ((yield) yield/k) ((set-yield-handler!) set-yield-handler!/k)
                     ((set-timer) set-timer/k)
                     ((enable-interrupts) enable-interrupts/k)
                     ((disable-interrupts) disable-interrupts/k)
                     ((panic) panic/k) ((set-panic-handler!) set-panic-handler!/k)
                     ((procedure-metadata) procedure-metadata/k) ((record?) record?/k) ((record) record/k)
                     ((record-type-descriptor) record-type-descriptor/k) ((record-ref) record-ref/k)
                     ((string->bytevector) string->bytevector/k)
                     ((bytevector->string) bytevector->string/k)
                     ((apply) apply/k)
                     ((values) values/k)
                     ((eq?) eq?/k) ((eqv?) eqv?/k) ((null?) null?/k) ((boolean?) boolean?/k)
                     ((procedure?) procedure?/k) ((symbol?) symbol?/k) ((string?) string?/k)
                     ((rational?) rational?/k) ((integer?) integer?/k)
                     ((f32?) f32?/k) ((f64?) f64?/k)
                     ((pair?) pair?/k) ((vector?) vector?/k) ((mvector?) mvector?/k)
                     ((bytevector?) bytevector?/k) ((mbytevector?) mbytevector?/k)
                     ((string->symbol) string->symbol/k) ((symbol->string) symbol->string/k)
                     ((cons) cons/k) ((car) car/k) ((cdr) cdr/k)
                     ((vector) vector/k) ((vector-length) vector-length/k) ((vector-ref) vector-ref/k)
                     ((make-mvector) make-mvector/k) ((mvector->vector) mvector->vector/k)
                     ((mvector-length) mvector-length/k)
                     ((mvector-ref) mvector-ref/k) ((mvector-set!) mvector-set!/k)
                     ((bytevector) bytevector/k) ((bytevector-length) bytevector-length/k)
                     ((bytevector-u8-ref) bytevector-u8-ref/k)
                     ((bytevector-u16-ref) bytevector-u16-ref/k)
                     ((bytevector-u32-ref) bytevector-u32-ref/k)
                     ((bytevector-u64-ref) bytevector-u64-ref/k)
                     ((make-mbytevector) make-mbytevector/k)
                     ((mbytevector->bytevector) mbytevector->bytevector/k)
                     ((mbytevector-length) mbytevector-length/k)
                     ((mbytevector-u8-ref) mbytevector-u8-ref/k)
                     ((mbytevector-u16-ref) mbytevector-u16-ref/k)
                     ((mbytevector-u32-ref) mbytevector-u32-ref/k)
                     ((mbytevector-u64-ref) mbytevector-u64-ref/k)
                     ((mbytevector-u8-set!) mbytevector-u8-set!/k)
                     ((mbytevector-u16-set!) mbytevector-u16-set!/k)
                     ((mbytevector-u32-set!) mbytevector-u32-set!/k)
                     ((mbytevector-u64-set!) mbytevector-u64-set!/k)
                     ((bitwise-arithmetic-shift-left) bitwise-arithmetic-shift-left/k)
                     ((bitwise-arithmetic-shift-right) bitwise-arithmetic-shift-right/k)
                     ((bitwise-not) bitwise-not/k) ((bitwise-and) bitwise-and/k)
                     ((bitwise-ior) bitwise-ior/k) ((bitwise-xor) bitwise-xor/k)
                     ((bitwise-length) bitwise-length/k) ((integer-floor-divmod) integer-floor-divmod/k)
                     ((numerator) numerator/k) ((denominator) denominator/k)
                     ((=) =/k) ((<=) <=/k) ((>=) >=/k) ((<) </k) ((>) >/k) ((+) +/k) ((-) -/k) ((*) */k) ((/) //k)
                     ((f32->rational) f32->rational/k) ((rational->f32) rational->f32/k)
                     ((f64->rational) f64->rational/k) ((rational->f64) rational->f64/k)
                     ((f32->u32) f32->u32/k) ((u32->f32) u32->f32/k)
                     ((f64->u64) f64->u64/k) ((u64->f64) u64->f64/k)
                     ((f32->f64) f32->f64/k) ((f64->f32) f64->f32/k)
                     ((f32-cmp) f32-cmp/k) ((f32-floor) f32-floor/k) ((f32-ceiling) f32-ceiling/k)
                     ((f32-truncate) f32-truncate/k) ((f32-round) f32-round/k)
                     ((f32+) f32+/k) ((f32-) f32-/k) ((f32*) f32*/k) ((f32/) f32//k)
                     ((f64-cmp) f64-cmp/k) ((f64-floor) f64-floor/k) ((f64-ceiling) f64-ceiling/k)
                     ((f64-truncate) f64-truncate/k) ((f64-round) f64-round/k)
                     ((f64+) f64+/k) ((f64-) f64-/k) ((f64*) f64*/k) ((f64/) f64//k)
                     (=> (lambda (name) (error "not a primitive" name))))))
            (lambda (renv k) (k p))))
         (else (error "not an expression" E)))))

   ;; TODO: control context operators need special treatment for k
   (define (current-control-context/k k . x*) (k (apply current-control-context x*)))
   (define (make-control-context/k k . x*) (k (apply make-control-context x*)))
   (define (call-with-escape-continuation/k k . x*) (k (apply call-with-escape-continuation x*)))
   (define (call-in-empty-context/k k . x*) (k (apply call-in-empty-context x*)))
   (define (control-context-register/k k . x*) (k (apply control-context-register x*)))
   (define (set-control-context-register!/k k . x*) (k (apply set-control-context-register! x*)))
   (define (yield/k k . x*) (k (apply yield x*)))
   (define (set-yield-handler!/k k . x*) (k (apply set-yield-handler! x*)))
   (define (set-timer/k k . x*) (k (apply set-timer x*)))
   (define (enable-interrupts/k k . x*) (k (apply enable-interrupts x*)))
   (define (disable-interrupts/k k . x*) (k (apply disable-interrupts x*)))
   (define (panic/k k . x*) (k (apply panic x*)))
   (define (set-panic-handler!/k k . x*) (k (apply set-panic-handler! x*)))
   (define (procedure-metadata/k k . x*) (k (apply procedure-metadata x*)))
   (define (record?/k k . x*) (k (apply record? x*)))
   (define (record/k k . x*) (k (apply record x*)))
   (define (record-type-descriptor/k k . x*) (k (apply record-type-descriptor x*)))
   (define (record-ref/k k . x*) (k (apply record-ref x*)))
   (define (string->bytevector/k k . x*) (k (apply string->bytevector x*)))
   (define (bytevector->string/k k . x*) (k (apply bytevector->string x*)))
   (define (eq?/k k . x*) (k (apply eq? x*)))
   (define (eqv?/k k . x*) (k (apply eqv? x*)))
   (define (null?/k k . x*) (k (apply null? x*)))
   (define (boolean?/k k . x*) (k (apply boolean? x*)))
   (define (procedure?/k k . x*) (k (apply procedure? x*)))
   (define (symbol?/k k . x*) (k (apply symbol? x*)))
   (define (string?/k k . x*) (k (apply string? x*)))
   (define (rational?/k k . x*) (k (apply rational? x*)))
   (define (integer?/k k . x*) (k (apply integer? x*)))
   (define (f32?/k k . x*) (k (apply f32? x*)))
   (define (f64?/k k . x*) (k (apply f64? x*)))
   (define (pair?/k k . x*) (k (apply pair? x*)))
   (define (vector?/k k . x*) (k (apply vector? x*)))
   (define (mvector?/k k . x*) (k (apply mvector? x*)))
   (define (bytevector?/k k . x*) (k (apply bytevector? x*)))
   (define (mbytevector?/k k . x*) (k (apply mbytevector? x*)))
   (define (string->symbol/k k . x*) (k (apply string->symbol x*)))
   (define (symbol->string/k k . x*) (k (apply symbol->string x*)))
   (define (cons/k k . x*) (k (apply cons x*)))
   (define (car/k k . x*) (k (apply car x*)))
   (define (cdr/k k . x*) (k (apply cdr x*)))
   (define (vector/k k . x*) (k (apply vector x*)))
   (define (vector-length/k k . x*) (k (apply vector-length x*)))
   (define (vector-ref/k k . x*) (k (apply vector-ref x*)))
   (define (make-mvector/k k . x*) (k (apply make-mvector x*)))
   (define (mvector->vector/k k . x*) (k (apply mvector->vector x*)))
   (define (mvector-length/k k . x*) (k (apply mvector-length x*)))
   (define (mvector-ref/k k . x*) (k (apply mvector-ref x*)))
   (define (mvector-set!/k k . x*) (k (apply mvector-set! x*)))
   (define (bytevector/k k . x*) (k (apply bytevector x*)))
   (define (bytevector-length/k k . x*) (k (apply bytevector-length x*)))
   (define (bytevector-u8-ref/k k . x*) (k (apply bytevector-u8-ref x*)))
   (define (bytevector-u16-ref/k k . x*) (k (apply bytevector-u16-ref x*)))
   (define (bytevector-u32-ref/k k . x*) (k (apply bytevector-u32-ref x*)))
   (define (bytevector-u64-ref/k k . x*) (k (apply bytevector-u64-ref x*)))
   (define (make-mbytevector/k k . x*) (k (apply make-mbytevector x*)))
   (define (mbytevector->bytevector/k k . x*) (k (apply mbytevector->bytevector x*)))
   (define (mbytevector-length/k k . x*) (k (apply mbytevector-length x*)))
   (define (mbytevector-u8-ref/k k . x*) (k (apply mbytevector-u8-ref x*)))
   (define (mbytevector-u16-ref/k k . x*) (k (apply mbytevector-u16-ref x*)))
   (define (mbytevector-u32-ref/k k . x*) (k (apply mbytevector-u32-ref x*)))
   (define (mbytevector-u64-ref/k k . x*) (k (apply mbytevector-u64-ref x*)))
   (define (mbytevector-u8-set!/k k . x*) (k (apply mbytevector-u8-set! x*)))
   (define (mbytevector-u16-set!/k k . x*) (k (apply mbytevector-u16-set! x*)))
   (define (mbytevector-u32-set!/k k . x*) (k (apply mbytevector-u32-set! x*)))
   (define (mbytevector-u64-set!/k k . x*) (k (apply mbytevector-u64-set! x*)))
   (define (bitwise-arithmetic-shift-left/k k . x*) (k (apply bitwise-arithmetic-shift-left x*)))
   (define (bitwise-arithmetic-shift-right/k k . x*) (k (apply bitwise-arithmetic-shift-right x*)))
   (define (bitwise-not/k k . x*) (k (apply bitwise-not x*)))
   (define (bitwise-and/k k . x*) (k (apply bitwise-and x*)))
   (define (bitwise-ior/k k . x*) (k (apply bitwise-ior x*)))
   (define (bitwise-xor/k k . x*) (k (apply bitwise-xor x*)))
   (define (bitwise-length/k k . x*) (k (apply bitwise-length x*)))
   (define (integer-floor-divmod/k k . x*) (k (apply integer-floor-divmod x*)))
   (define (numerator/k k . x*) (k (apply numerator x*)))
   (define (denominator/k k . x*) (k (apply denominator x*)))
   (define (=/k k . x*) (k (apply = x*)))
   (define (<=/k k . x*) (k (apply <= x*)))
   (define (>=/k k . x*) (k (apply >= x*)))
   (define (</k k . x*) (k (apply < x*)))
   (define (>/k k . x*) (k (apply > x*)))
   (define (+/k k . x*) (k (apply + x*)))
   (define (-/k k . x*) (k (apply - x*)))
   (define (*/k k . x*) (k (apply * x*)))
   (define (//k k . x*) (k (apply / x*)))
   (define (f32->rational/k k . x*) (k (apply f32->rational x*)))
   (define (rational->f32/k k . x*) (k (apply rational->f32 x*)))
   (define (f64->rational/k k . x*) (k (apply f64->rational x*)))
   (define (rational->f64/k k . x*) (k (apply rational->f64 x*)))
   (define (f32->u32/k k . x*) (k (apply f32->u32 x*)))
   (define (u32->f32/k k . x*) (k (apply u32->f32 x*)))
   (define (f64->u64/k k . x*) (k (apply f64->u64 x*)))
   (define (u64->f64/k k . x*) (k (apply u64->f64 x*)))
   (define (f32->f64/k k . x*) (k (apply f32->f64 x*)))
   (define (f64->f32/k k . x*) (k (apply f64->f32 x*)))
   (define (f32-cmp/k k . x*) (k (apply f32-cmp x*)))
   (define (f32-floor/k k . x*) (k (apply f32-floor x*)))
   (define (f32-ceiling/k k . x*) (k (apply f32-ceiling x*)))
   (define (f32-truncate/k k . x*) (k (apply f32-truncate x*)))
   (define (f32-round/k k . x*) (k (apply f32-round x*)))
   (define (f32+/k k . x*) (k (apply f32+ x*)))
   (define (f32-/k k . x*) (k (apply f32- x*)))
   (define (f32*/k k . x*) (k (apply f32* x*)))
   (define (f32//k k . x*) (k (apply f32/ x*)))
   (define (f64-cmp/k k . x*) (k (apply f64-cmp x*)))
   (define (f64-floor/k k . x*) (k (apply f64-floor x*)))
   (define (f64-ceiling/k k . x*) (k (apply f64-ceiling x*)))
   (define (f64-truncate/k k . x*) (k (apply f64-truncate x*)))
   (define (f64-round/k k . x*) (k (apply f64-round x*)))
   (define (f64+/k k . x*) (k (apply f64+ x*)))
   (define (f64-/k k . x*) (k (apply f64- x*)))
   (define (f64*/k k . x*) (k (apply f64* x*)))
   (define (f64//k k . x*) (k (apply f64/ x*)))
   (define (apply/k            k proc arg*) (apply proc k arg*))
   (define (values/k           k . x*)      (apply k x*)))
  (define (E-eval E) ((E-stage E cenv.empty) renv.empty values)))

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
