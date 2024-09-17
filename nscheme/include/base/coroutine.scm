(splicing-local
  ((define current-coroutine-receive (make-parameter (lambda () (thread-wait (current-thread))))))
  (define (make-coroutine proc)
    (let* ((ch.in (make-channel))
           (t     (thread/suspend-to-kill
                    (lambda ()
                      (current-coroutine-receive
                        (lambda () (channel-get ch.in))
                        (lambda () (proc (channel-get ch.in))))))))
      (lambda (x)
        (thread (lambda () (channel-put ch.in x)))
        (thread-resume t (current-thread))
        ((current-coroutine-receive))))))
