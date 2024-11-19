(splicing-local
  ((define current-coroutine-receive (make-parameter (lambda () (sync)))))
  (define (make-coroutine proc)
    (let* ((ch.in (make-channel))
           (t     (thread (lambda () (current-coroutine-receive
                                       (lambda () (channel-get ch.in))
                                       (lambda () (proc (channel-get ch.in)))))))
           (dead  (handle-evt (thread-dead-evt t) (lambda (_) (error "dead coroutine")))))
      (lambda (x)
        (sync (channel-put-evt ch.in x) dead)
        ((current-coroutine-receive))))))
