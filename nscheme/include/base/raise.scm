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
  ;;     (let ((tag (make-prompt-tag)))
  ;;       (escape-prompt tag
  ;;         handle
  ;;         (with-raise-handler
  ;;           (lambda (exn) (when (catch? exn) (escape-to tag exn)))
  ;;           thunk)))
  (thunk))

(define (make-restart name desc effector) (vector name desc effector))
(define (restart-name        r) (vector-ref r 0))
(define (restart-description r) (vector-ref r 1))
(define (restart-effector    r) (vector-ref r 2))

(define (with-restart name desc effector thunk)
  ;; TODO:
  ;; - set up escape prompt
  ;; - compose initial effector with continuation to produce final effector, packaged with restart
  ;;   - the initial effector should be called in the dynamic env of the with-restart call, i.e.,
  ;;     after escaping to the prompt, not in the dynamic env of the invoke-restart call before
  ;;     escaping.
  ;; - install restart in dynamic env
  ;; - something like this:
  ;;   (let ((tag (make-prompt-tag)))
  ;;     (escape-prompt tag
  ;;       effector
  ;;       (parameterize ((bound-restarts
  ;;                        (cons (make-restart name desc (lambda arg* (apply escape-to tag arg*)))
  ;;                              (bound-restarts))))
  ;;         (thunk))))
  (thunk))

(define (with-simple-restart name desc thunk) (with-restart name desc (lambda () (values)) thunk))

(define (with-restart:abort desc thunk) (with-simple-restart 'abort desc thunk))
(define (with-restart:continue desc thunk) (with-simple-restart 'continue desc thunk))
(define (with-restart:retry desc thunk) (let loop () (with-restart 'retry loop thunk)))
(define (with-restart:use-value desc thunk) (with-restart 'use-value desc (lambda (x) x) thunk))
(define (with-restart:use-values desc thunk) (with-restart 'use-values desc values thunk))

(define (bound-restarts) (error "TODO: bound-restarts"))
(define (find-restart name)
  (let ((r* (memp (lambda (r) (equal? (restart-name r) name)) (bound-restarts))))
    (and r* (car r*))))
(define (invoke-restart name . arg*)
  (let ((r (find-restart name)))
    (unless r (error "no restart to invoke" name arg*))
    (apply (restart-effector r) arg*)))

;; TODO:
;(define (bound-raise-handlers) (error "TODO: bound-raise-handlers"))
;(define (raise-continuable x)
;  (let loop ((h* (bound-raise-handlers)))
;    (unless (null? h*)
;      (parameterize ((bound-raise-handlers (cdr h*)))
;        ((car h*) x)
;        (loop (cdr h*))))))
(define (raise-continuable x) (error 'raise x))
(define (raise             x) (raise-continuable x) (panic 'raise x))
(define (raise/continue    x) (with-restart:continue 'raise/continue (lambda () (raise x))))
