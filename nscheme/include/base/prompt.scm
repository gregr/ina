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
  (let* ((ch     (make-channel))
         (escape (lambda x*
                   (channel-put ch (lambda () (apply on-escape x*)))
                   (thread-wait (current-thread)))))
    ((with-local-custodian
       (lambda ()
         (thread (lambda () (let-values ((x* (proc escape)))
                              (channel-put ch (lambda () (apply values x*))))))
         (channel-get ch))))))

(define (with-isolation on-panic thunk)
  (with-escape
    on-panic
    (lambda (escape)
      (panic-handler
        escape
        (lambda () (without-restarts (lambda () (without-raise-handlers thunk))))))))

(define (isolated-thread on-panic thunk)
  (thread (lambda () (with-isolation on-panic thunk))))

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
  (define (without-restarts thunk) (raw-current-restart* '() thunk))
  (define (with-raw-restart name desc effector thunk)
    (raw-current-restart* (cons (make-restart name desc effector) (raw-current-restart*)) thunk))
  (define (with-raw-restart* nde* thunk)
    (raw-current-restart* (append (map (lambda (nde) (apply make-restart nde)) nde*)
                                  (raw-current-restart*))
                          thunk)))

(define (with-restart name desc effector thunk)
  (with-escape effector (lambda (escape) (with-raw-restart name desc escape thunk))))
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

(define (with-abort      desc on-abort thunk) (with-restart 'abort desc on-abort thunk))
(define (with-continue   desc          thunk) (with-restart 'continue desc (lambda x* (values))
                                                            thunk))
(define (with-retry      desc          thunk) (let loop () (with-restart 'retry desc loop thunk)))
(define (with-use-value  desc          thunk) (with-restart 'use-value desc (lambda (x) x) thunk))
(define (with-use-values desc          thunk) (with-restart 'use-values desc values thunk))

(define (with-continue-alternative* desc thunk . thunk*)
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

(define (abort . x*)      (apply invoke-restart 'abort x*) (panic 'abort x*))
(define (continue)        (invoke-restart 'continue))
(define (retry)           (invoke-restart 'retry))
(define (use-value x)     (invoke-restart 'use-value x))
(define (use-values . x*) (apply invoke-restart 'use-values x*))

;;;;;;;;;;;;;;;;;;;;;;
;;; Raise handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;
(splicing-local
  ((define current-raise-handler* (make-parameter '())))
  (define (without-raise-handlers thunk) (current-raise-handler* '() thunk))
  (define (with-raise-handler handle thunk)
    (current-raise-handler* (cons handle (current-raise-handler*)) thunk))
  (define (with-raise-handler* handle* thunk)
    (current-raise-handler* (append handle* (current-raise-handler*))) thunk)
  (define (raise-continuable x)
    (let loop ((h* (current-raise-handler*)))
      (unless (null? h*)
        (current-raise-handler*
          (cdr h*)
          (lambda ()
            ((car h*) x)
            (loop (cdr h*))))))))
(define (raise          x) (raise-continuable x) (panic 'raise x))
(define (raise/continue x) (with-continue 'raise/continue (lambda () (raise x))))

(define (with-catch catch? handle thunk)
  (with-escape handle (lambda (escape)
                        (with-raise-handler (lambda (exn) (when (catch? exn) (escape exn)))
                                            thunk))))
(define (with-catch* catch?handle* thunk)
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
