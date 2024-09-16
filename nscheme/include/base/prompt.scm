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
