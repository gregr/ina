(define (E-compile-rkt E global-addr=>id)
  ;; NOTE: we assume that suffixing variable names with .N will prevent them from colliding with
  ;; any global names, such as the names of any primitives or special form keywords.
  (define address->fresh-id (address->local-gensym/default '_))
  (define (cenv-extend cenv addr* id*) (append (map cons addr* id*) cenv))
  (define (cenv-ref cenv addr)
    (let ((addr&id (assv addr cenv)))
      (unless addr&id (mistake 'E-compile-rkt "unbound or out-of-phase reference" addr))
      (cdr addr&id)))
  (let loop/env ((E E) (cenv global-addr=>id))
    (define (loop E) (loop/env E cenv))
    ;; TODO: optionally generate error-checking code for operations that could fail
    ;; - use E-note to provide source location for error messages
    ;; - optionally build a stack trace that retains thread lineage unless explicitly isolated
    ;;   - stack trace of a new thread should begin at the parent thread's current trace
    (cond
      ((E:quote?        E) (list 'quote (E:quote-value E)))
      ((E:ref?          E) (cenv-ref cenv (E:ref-address E)))
      ((E:if?           E) (list 'if (loop (E:if-condition E)) (loop (E:if-consequent E))
                                 (loop (E:if-alternative E))))
      ((E:call?         E) (cons (loop (E:call-operator E)) (map loop (E:call-operand* E))))
      ((E:apply/values? E) (list 'call-with-values
                                 (list 'lambda '() (loop (E:apply/values-operand E)))
                                 (loop (E:apply/values-operator E))))
      ;; TODO: generate Racket code to wrap case-lambda with inspector-compatible metadata
      ((E:case-lambda?  E) (let* ((param*~* (E:case-lambda-param*~* E))
                                  (^body*   (map (lambda (addr* body)
                                                   (lambda id*
                                                     (loop/env body (cenv-extend cenv addr* id*))))
                                                 (map improper-list->list param*~*)
                                                 (E:case-lambda-body* E))))
                             (let* ((id*~* (map (lambda (param*~)
                                                  (improper-list-map address->fresh-id param*~))
                                                param*~*))
                                    (body* (map apply ^body* (map improper-list->list id*~*))))
                               (cons 'case-lambda (map list id*~* body*)))))
      ((E:letrec?       E) (let* ((lhs*       (E:letrec-binding-left* E))
                                  (^rhs*&body (lambda id*
                                                (let ((cenv (cenv-extend cenv lhs* id*)))
                                                  (values (map (lambda (rhs) (loop/env rhs cenv))
                                                               (E:letrec-binding-right* E))
                                                          (loop/env (E:letrec-body E) cenv))))))
                             (let ((id* (map address->fresh-id lhs*)))
                               (let-values (((rhs* body) (apply ^rhs*&body id*)))
                                 (list 'letrec (map list id* rhs*) body)))))
      (else                (mistake 'E-compile-rkt "not an E" E)))))
