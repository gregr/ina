(splicing-local
  ((splicing-let ((offset.dynamic-env          0)
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

   (define (with-untagged-escape-prompt on-escape thunk)
     (let ((saved-denv (dynamic-env)))
       (with-raw-escape-prompt
         (lambda x*
           (dynamic-env saved-denv)
           (apply on-escape x*))
         thunk))))

  ;; This call initializes dynamic-extent state slots for the current native thread.
  ;; If/when another native thread is started, it should call this initializer before continuing.
  (initialize-native-thread-local-state!)

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

  (define escape-to-prompt raw-escape-to-prompt))

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

;; TODO: make-coroutine using make-empty-dynamic-extent (primitive using Racket threads)
