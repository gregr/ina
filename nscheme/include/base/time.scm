(define (sleep-seconds sec) (sleep-seconds-nanoseconds sec 0))

(define current-time-utc                    (current-time/type 'utc))
(define current-time-monotonic              (current-time/type 'monotonic))
(define current-time-process                (current-time/type 'process))
(define current-time-thread                 (current-time/type 'thread))
(define current-time-garbage-collector-cpu  (current-time/type 'garbage-collector-cpu))
(define current-time-garbage-collector-real (current-time/type 'garbage-collector-real))

(define (current-seconds-utc) (let-values (((sec nsec) (current-time-utc))) sec))
