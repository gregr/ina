(splicing-local
  ((define current-coroutine-receive (make-parameter (lambda () (sync never-evt)))))
  (define (make-coroutine proc)
    (let* ((ch.in (make-channel))
           (t     (thread/suspend-to-kill
                    (lambda ()
                      (current-coroutine-receive
                        (lambda () (channel-get ch.in))
                        (lambda () (proc (channel-get ch.in))))))))
      (lambda (x)
        (thread-resume t (current-thread))
        (channel-put ch.in x)
        ((current-coroutine-receive))))))
