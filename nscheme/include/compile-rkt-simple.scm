(define (E-compile-rkt E)
  (define address->fresh-id
    (let ((&count (box 0)))
      (lambda (a)
        (let ((name  (let ((name? (and (vector? a) (<= 1 (vector-length a)) (vector-ref a 0))))
                       (if (identifier? name?) (syntax-unwrap name?) 'X)))
              (count (unbox &count)))
          (set-box! &count (+ count 1))
          (string->symbol (string-append (symbol->string name) "." (number->string count)))))))
  (define ($rkt:quote                     x) (list 'quote x))
  (define ($rkt:if                    c t f) (list 'if c t f))
  (define ($rkt:call            rator rand*) (cons rator rand*))
  (define ($rkt:apply/values    rator vrand) (list 'apply/values rator vrand))
  (define ($rkt:case-lambda param* ^body*)
    (let* ((id*~* (map (lambda (param) (improper-list-map address->fresh-id param)) param*))
           (body* (map apply ^body* (map improper-list->list id*~*))))
      (cons 'case-lambda (map list id*~* body*))))
  (define ($rkt:letrec lhs* ^rhs*&body) (let ((id* (map address->fresh-id lhs*)))
                                          (let-values (((rhs* body) (apply ^rhs*&body id*)))
                                            (list 'letrec (map list id* rhs*) body))))
  (define cenv.empty '())
  (define (cenv-ref    cenv addr)      (cdr (assq addr cenv)))
  (define (cenv-extend cenv addr* id*) (append (map cons addr* id*) cenv))
  (let loop/env ((E E) (env cenv.empty))
    (define (loop E) (loop/env E env))
    (cond
      ((E:annotated?    E) (loop (E:annotated-E E)))
      ((E:quote?        E) ($rkt:quote (E:quote-value E)))
      ((E:ref?          E) (cenv-ref env (E:ref-address E)))
      ((E:if?           E) ($rkt:if (loop (E:if-condition E))
                                    (loop (E:if-consequent E))
                                    (loop (E:if-alternative E))))
      ((E:call?         E) ($rkt:call (loop (E:call-operator E)) (map loop (E:call-operand* E))))
      ((E:apply/values? E) ($rkt:apply/values (loop (E:apply/values-operator E))
                                              (loop (E:apply/values-operand E))))
      ((E:case-lambda?  E) (let* ((param* (E:case-lambda-param* E))
                                  (^body* (map (lambda (addr* body)
                                                 (lambda id*
                                                   (loop/env body (cenv-extend env addr* id*))))
                                               (map improper-list->list param*)
                                               (E:case-lambda-body* E))))
                             ($rkt:case-lambda param* ^body*)))
      ((E:letrec?       E) (let* ((lhs*       (E:letrec-binding-left* E))
                                  (^rhs*&body (lambda id*
                                                (let ((env (cenv-extend env lhs* id*)))
                                                  (values (map (lambda (rhs) (loop/env rhs env))
                                                               (E:letrec-binding-right* E))
                                                          (loop/env (E:letrec-body E) env))))))
                             ($rkt:letrec lhs* ^rhs*&body)))
      ((E:prim?         E) (E:prim-name E))
      (else                (error "not an expression" E)))))
