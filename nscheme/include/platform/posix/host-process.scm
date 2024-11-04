(define (host-process-in        p) (p 'in))
(define (host-process-out       p) (p 'out))
(define (host-process-err       p) (p 'err))
(define (host-process-pid       p) (p 'pid))
(define (host-process-wait      p) (p 'wait))
(define (host-process-kill      p) (p 'kill))
(define (host-process-interrupt p) (p 'interrupt))
(define (host-process in out err path arg* env)
  (host-process/k in out err path arg* env raise-io-error raise-io-error values))
(define (host-process/k in out err path arg* env handle-internal-error kf k)
  (define (x->fd x)
    (and x (let ((kv (assoc 'file-descriptor (port-description x))))
             (and kv (cdr kv)))))
  (raw-host-process/k
    (x->fd in) (x->fd out) (if (and err (or (eqv? out err) (eq? err 'stdout)))
                               'stdout
                               (x->fd err))
    path arg* env kf
    (lambda (p)
      (let ((in.p  (host-process-in  p))
            (out.p (host-process-out p))
            (err.p (host-process-err p)))
        (mlet ((fuse* '()))
          (define (fuse-io in out close!)
            (thread
              (lambda ()
                (let* ((buffer-size 4096) (buffer (make-mbytevector buffer-size 0)))
                  (let loop ()
                    (iport-read/k
                      in buffer 0 1 buffer-size handle-internal-error close!
                      (lambda (amount)
                        (oport-write/k out buffer 0 amount amount handle-internal-error
                                       (lambda (amount) (loop))))))))))
          (define (fuse*-push t) (set! fuse* (cons t fuse*)) #f)
          (define (fuse-input in out)
            (fuse-io in out (lambda () (oport-close/k out handle-internal-error values))))
          (define (fuse-output in out)
            (fuse*-push
              (fuse-io in out (lambda () (iport-close/k in handle-internal-error values)))))
          (let ((out (and out.p (if out (fuse-output out.p out) out.p)))
                (err (and err.p (if err (fuse-output err.p err) err.p)))
                (in  (and in.p
                          (if in
                              (let ((cust (make-custodian)))
                                (let ((t.in (current-custodian cust (lambda ()
                                                                      (fuse-input in in.p)))))
                                  (fuse*-push
                                    (thread
                                      (lambda ()
                                        (p 'wait)
                                        (custodian-shutdown-all cust)
                                        (thread-wait t.in)
                                        (oport-close/k in.p handle-internal-error values))))))
                              in.p))))
            (k (if (null? fuse*)
                   p
                   (mlet ((status #f))
                     (set! status (thread (lambda () (let ((exit-code (p 'wait)))
                                                       (for-each thread-wait fuse*)
                                                       (set! status exit-code)
                                                       (set! fuse* '())))))
                     (lambda (method)
                       (case method
                         ((in)   in)
                         ((out)  out)
                         ((err)  err)
                         ((wait) (let ((current status))
                                   (if (number? current) current (begin (thread-wait current)
                                                                        status))))
                         (else   (p method)))))))))))))
