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
    (E-case
      E (lambda (E) (mistake 'E-compile-rkt "not an E" E))
      E:quote?        (lambda (v)     (list 'quote v))
      E:ref?          (lambda (addr)  (cenv-ref cenv addr))
      E:if?           (lambda (c t f) (list 'if (loop c) (loop t) (loop f)))
      E:call?         (lambda (rator rand*) (cons (loop rator) (map loop rand*)))
      E:apply/values? (lambda (rator vrand) (list 'call-with-values (list 'lambda '() (loop vrand))
                                                  (loop rator)))
      ;; TODO: generate Racket code to wrap case-lambda with inspector-compatible metadata
      E:case-lambda?  (lambda (param*~* body*)
                        (cons 'case-lambda
                              (map (lambda (param*~ body)
                                     (let ((id*~ (improper-list-map address->fresh-id param*~)))
                                       (list id*~ (loop/env body (cenv-extend cenv
                                                                              (improper-list->list param*~)
                                                                              (improper-list->list id*~))))))
                                   param*~* body*)))
      E:letrec?       (lambda (lhs* rhs* body)
                        (let ((id* (map address->fresh-id lhs*)))
                          (let-values (((rhs* body) (let ((cenv (cenv-extend cenv lhs* id*)))
                                                      (values (map (lambda (rhs) (loop/env rhs cenv)) rhs*)
                                                              (loop/env body cenv)))))
                            (list 'letrec (map list id* rhs*) body)))))))
