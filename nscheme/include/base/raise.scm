(define current-raise-handler* (make-dynamic-parameter '()))
;; TODO:
;(define (raise-continuable x)
;  (let loop ((h* (current-raise-handler*)))
;    (unless (null? h*)
;      (with-dynamic-binding
;        current-raise-handler* (cdr h*)
;        (lambda ()
;          ((car h*) x)
;          (loop (cdr h*)))))))
(define (raise-continuable x) (error 'raise x))
(define (raise             x) (raise-continuable x) (panic 'raise x))
(define (raise/continue    x) (with-restart:continue 'raise/continue (lambda () (raise x))))

(define (with-raise-handler handle thunk)
  ;; TODO:
  ;; - install handle in dynamic env
  (thunk))

(define (with-raise-handler:catch catch? handle thunk)
  ;; TODO:
  ;; - set up escape prompt
  ;; - install raise-handler in dynamic env
  ;;   - this handler will first use catch? to decide whether an exception is applicable
  ;;   - if applicable, escape to the prompt and return the result of (handle exn)
  ;;   - something like this:
  ;;     (let ((tag (make-escape-prompt-tag)))
  ;;       (with-escape-prompt tag
  ;;         handle
  ;;         (with-raise-handler
  ;;           (lambda (exn) (when (catch? exn) (escape-to tag exn)))
  ;;           thunk)))
  (thunk))
