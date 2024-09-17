(define (with-local-custodian thunk)
  (let ((cust (make-custodian)))
    (let-values ((x* (current-custodian
                       cust
                       (lambda ()
                         (let ((parent (current-thread)))
                           (thread (lambda () (thread-wait parent) (custodian-shutdown-all cust))))
                         (thunk)))))
      (custodian-shutdown-all cust)
      (apply values x*))))

(define (with-escape on-escape proc)
  (let* ((ch.return (make-channel))
         (escape    (lambda (x)
                      (channel-put ch.return (lambda () (on-escape x)))
                      (thread-wait (current-thread))))
         (^return   (with-local-custodian
                      (lambda ()
                        (thread (lambda ()
                                  (let-values ((x* (proc escape)))
                                    (channel-put ch.return (lambda () (apply values x*))))))
                        (channel-get ch.return)))))
    (^return)))

;;;;;;;;;;;;;;;;
;;; Restarts ;;;
;;;;;;;;;;;;;;;;
(define (make-restart name desc effector) (vector name desc effector))
(define (restart-name        r) (vector-ref r 0))
(define (restart-description r) (vector-ref r 1))
(define (restart-effector    r) (vector-ref r 2))

(splicing-local
  ((define raw-current-restart* (make-parameter '())))
  (define (current-restart*) (raw-current-restart*))
  (define (with-raw-restart name desc effector thunk)
    (raw-current-restart* (cons (make-restart name desc effector) (raw-current-restart*)) thunk))
  (define (with-raw-restart* nde* thunk)
    (raw-current-restart* (append (map (lambda (nde) (apply make-restart nde)) nde*)
                                  (raw-current-restart*))
                          thunk)))

(define (with-restart name desc effector thunk)
  (with-escape
    (lambda (effector) (effector))
    (lambda (escape)
      (with-raw-restart name desc (lambda x* (escape (lambda () (apply effector x*)))) thunk))))
(define (with-restart* nde* thunk)
  (with-escape
    (lambda (effector) (effector))
    (lambda (escape)
      (with-raw-restart*
        (map (lambda (nde)
               (apply (lambda (name desc effector)
                        (list name desc (lambda x* (escape (lambda () (apply effector x*))))))
                      nde))
             nde*)
        thunk))))
(define (with-simple-restart name desc thunk) (with-restart name desc (lambda () (values)) thunk))

(define (with-restart:abort desc thunk) (with-simple-restart 'abort desc thunk))
(define (with-restart:continue desc thunk) (with-simple-restart 'continue desc thunk))
(define (with-restart:retry desc thunk) (let loop () (with-restart 'retry loop thunk)))
(define (with-restart:use-value desc thunk) (with-restart 'use-value desc (lambda (x) x) thunk))
(define (with-restart:use-values desc thunk) (with-restart 'use-values desc values thunk))

(define (with-restart:continue/choice* desc thunk . thunk*)
  (let loop ((thunk thunk) (thunk* thunk*))
    (if (null? thunk*)
        (thunk)
        (with-restart 'continue desc
                      (lambda () (loop (car thunk*) (cdr thunk*)))
                      thunk))))

(define (find-restart name)
  (let ((r* (memp (lambda (r) (equal? (restart-name r) name)) (current-restart*))))
    (and r* (car r*))))
(define (invoke-restart name . arg*)
  (let ((r (find-restart name)))
    (when r (apply (restart-effector r) arg*))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Raise handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;
(define current-raise-handler* (make-parameter '()))
(define (raise-continuable x)
  (let loop ((h* (current-raise-handler*)))
    (unless (null? h*)
      (current-raise-handler*
        (cdr h*)
        (lambda ()
          ((car h*) x)
          (loop (cdr h*)))))))
(define (raise          x) (raise-continuable x) (panic 'raise x))
(define (raise/continue x) (with-restart:continue 'raise/continue (lambda () (raise x))))

(define (with-raise-handler handle thunk)
  (current-raise-handler* (cons handle (current-raise-handler*)) thunk))
(define (with-raise-handler* handle* thunk)
  (current-raise-handler* (append handle* (current-raise-handler*))) thunk)

(define (with-raise-handler:catch catch? handle thunk)
  (with-escape
    handle
    (lambda (escape)
      (with-raise-handler
        (lambda (exn) (when (catch? exn) (escape exn)))
        thunk))))
(define (with-raise-handler:catch* catch?handle* thunk)
  (with-escape
    (lambda (handle-exn) (handle-exn))
    (lambda (escape)
      (with-raise-handler*
        (map (lambda (catch?handle)
               (apply (lambda (catch? handle)
                        (lambda (exn) (when (catch? exn) (escape (lambda () (handle exn))))))
                      catch?handle))
             catch?handle*)
        thunk))))
