(define (with-untagged-escape-prompt on-escape thunk)
  (let ((saved-denv (dynamic-env)))
    (with-raw-escape-prompt
      (lambda x*
        (dynamic-env saved-denv)
        (apply on-escape x*))
      thunk)))

(define (with-tagged-escape-prompt tag.prompt on-escape thunk)
  (with-untagged-escape-prompt
    (lambda (tag.requested . x*)
      (if (eq? tag.requested tag.prompt)
          (apply on-escape x*)
          (apply escape-to-prompt tag.requested x*)))
    thunk))

(define make-escape-prompt-tag
  (case-lambda
    (()     (make-mvector 0 0))
    ((name) (let ((tag (make-mvector 1 0))) (mvector-set! tag 0 name)))))

(define (with-finally final-thunk thunk)
  (let-values ((result*
                 (with-untagged-escape-prompt
                   (lambda (tag . x*)
                     (final-thunk)
                     (apply escape-to-prompt tag x*))
                   thunk)))
    (final-thunk)
    (apply values result*)))

;; TODO: make-coroutine using make-empty-dynamic-extent (primitive using Racket threads)
