(define current-input-port  (make-parameter (thread-safe-iport standard-input-port)))
(define current-output-port (make-parameter (thread-safe-oport standard-output-port)))
(define current-error-port  (make-parameter (thread-safe-oport standard-error-port)))
