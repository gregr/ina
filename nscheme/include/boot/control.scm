(splicing-local
  ((splicing-let ((offset.ntst.dynamic-env     0)
                  (offset.ntst.coroutine-state 1)
                  (offset.cst.controller  0)
                  (offset.cst.current-raw 1))
     (define dynamic-env
       (case-lambda
         (()  (mvector-ref  (native-thread-local-register) offset.ntst.dynamic-env))
         ((x) (mvector-set! (native-thread-local-register) offset.ntst.dynamic-env x))))
     (define current-coroutine-state
       (case-lambda
         (()  (mvector-ref  (native-thread-local-register) offset.ntst.coroutine-state))
         ((x) (mvector-set! (native-thread-local-register) offset.ntst.coroutine-state x))))

     (define (make-coroutine-state rcr)
       (let* ((cst        (make-mvector 2 0))
              (controller (lambda x* (with-dynamic-env-extend
                                       (lambda () (current-coroutine-state cst)
                                         (apply (coroutine-state-current-raw cst) x*))))))
         (set-coroutine-state-controller!  cst controller)
         (set-coroutine-state-current-raw! cst rcr)
         cst))
     (define (set-coroutine-state-controller!  st ctrl) (mvector-set! st offset.cst.controller  ctrl))
     (define (set-coroutine-state-current-raw! st rcr)  (mvector-set! st offset.cst.current-raw rcr))
     (define (coroutine-state-controller  st) (mvector-ref st offset.cst.controller))
     (define (coroutine-state-current-raw st) (mvector-ref st offset.cst.current-raw))

     (define (initialize-native-thread-state!)
       (let ((native-thread-state.initial (make-mvector 2 0))
             (cst                         (make-coroutine-state (current-raw-coroutine))))
         (mvector-set! native-thread-state.initial offset.ntst.dynamic-env '())
         (mvector-set! native-thread-state.initial offset.ntst.coroutine-state cst)
         (native-thread-local-register native-thread-state.initial))))

   (define (with-untagged-escape-prompt on-escape thunk)
     (let ((saved-denv (dynamic-env)))
       (with-raw-escape-prompt
         (lambda x*
           (dynamic-env saved-denv)
           (apply on-escape x*))
         thunk))))

  ;; This call initializes dynamic-extent state slots for the current native thread.
  ;; If/when another native thread is started, it should call this initializer before continuing.
  (initialize-native-thread-state!)

  (define (dynamic-env-ref key default-value)
    (let loop ((frame* (dynamic-env)))
      (cond ((pair?    frame*) (let ((frame (car frame*)))
                                 (if (eq? (car frame) key)
                                     (cdr frame)
                                     (loop (cdr frame*)))))
            ((mvector? frame*) (loop (mvector-ref frame* 0)))  ; follow dynamic parent link
            (else              default-value))))

  (define (dynamic-env-set! key value)
    (dynamic-env
      (cons (cons key value)
            (let* ((frame* (dynamic-env))
                   (depth  (let loop ((frame* frame*) (depth 0))  ; find any existing frame for key
                             (and (pair? frame*)
                                  (let ((frame (car frame*)))
                                    (if (eq? (car frame) key)
                                        depth
                                        (loop (cdr frame*) (+ depth 1))))))))
              (if depth
                  (let loop ((frame* frame*) (depth depth))  ; remove the existing frame
                    (if (< 0 depth)
                        (cons (car frame*) (loop (cdr frame*) (- depth 1)))
                        (cdr frame*)))
                  frame*)))))

  (define (with-dynamic-env-clear thunk)
    (let ((saved-denv (dynamic-env)))
      (dynamic-env '())
      (let-values ((result* (thunk)))
        (dynamic-env saved-denv)
        (apply values result*))))

  (define (with-dynamic-env-extend thunk)
    (let ((saved-denv (dynamic-env)))
      (let-values ((result* (thunk)))
        (dynamic-env saved-denv)
        (apply values result*))))

  (define (with-escape-prompt tag.prompt on-escape thunk)
    (with-untagged-escape-prompt
      (lambda (tag.requested . x*)
        (if (eq? tag.requested tag.prompt)
            (apply on-escape x*)
            (apply escape-to-prompt tag.requested x*)))
      thunk))

  (define (with-finally final-thunk thunk)
    (let-values ((result*
                   (with-untagged-escape-prompt
                     (lambda (tag . x*)
                       (final-thunk)
                       (apply escape-to-prompt tag x*))
                     thunk)))
      (final-thunk)
      (apply values result*)))

  (define (current-coroutine) (coroutine-state-controller (current-coroutine-state)))

  (define (make-coroutine proc)
    (let* ((rcr (make-raw-coroutine
                  (lambda x* (with-dynamic-env-clear (lambda () (apply proc x*))))))
           (cst (make-coroutine-state rcr)))
      (coroutine-state-controller cst)))
  )

(define escape-to-prompt raw-escape-to-prompt)

(define make-escape-prompt-tag
  (case-lambda
    (()     (make-mvector 0 0))
    ((name) (let ((tag (make-mvector 1 0))) (mvector-set! tag 0 name)))))

(define (make-dynamic-parameter default-value)
  (let ((key (make-mvector 0 0)))
    (define param
      (case-lambda
        (()  (dynamic-env-ref  key default-value))
        ((x) (dynamic-env-set! key x))))
    param))

;; TODO: make-generator
;; - asymmetric, hierarchical coroutines
;; - dynamic-env is cleared, like in make-coroutine

;; TODO: make-thread
;; - implements structured concurrency
;;   - life is bound to a scheduler whose dynamic-extent is fixed
;; - dynamic-env does not need to be cleared
;;   - unlike with coroutines and generators, this is safe from race-conditions because the
;;     scheduler is stationary
