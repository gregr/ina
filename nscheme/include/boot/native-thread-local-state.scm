(splicing-let ((offset.dynamic-env          0)
               (offset.virtual-thread-state 1))
  (define dynamic-env
    (case-lambda
      (()  (mvector-ref  (native-thread-local-register) offset.dynamic-env))
      ((x) (mvector-set! (native-thread-local-register) offset.dynamic-env x))))
  (define virtual-thread-state
    (case-lambda
      (()  (mvector-ref  (native-thread-local-register) offset.virtual-thread-state))
      ((x) (mvector-set! (native-thread-local-register) offset.virtual-thread-state x))))
  (define (initialize-native-thread-local-state!)
    (let ((dynamic-state.initial (make-mvector 2 0)))
      (mvector-set! dynamic-state.initial offset.dynamic-env '())
      (mvector-set! dynamic-state.initial offset.virtual-thread-state #f)
      (native-thread-local-register dynamic-state.initial))))

;; This call initializes dynamic-extent state slots for the current native thread.
;; If/when another native thread is started, it should call this initializer before continuing.
(initialize-native-thread-local-state!)
