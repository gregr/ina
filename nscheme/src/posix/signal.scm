(define SIGHUP   1)
(define SIGINT   2)
(define SIGTERM 15)

(define (posix-set-signal-handler! signal handler)
  ((current-posix-set-signal-handler!) signal handler))

(define exit
  (case-lambda
    (()     ((current-posix-exit) 0))
    ((code) ((current-posix-exit) code))))
