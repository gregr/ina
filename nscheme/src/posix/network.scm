(define (tcp-listen/k  host port reuse? max-backlog    kf k) (((current-posix-network) 'tcp-listen  host port reuse? max-backlog)    kf k))
(define (tcp-connect/k host port local-host local-port kf k) (((current-posix-network) 'tcp-connect host port local-host local-port) kf k))
(define (udp-open/k    family-host family-port         kf k) (((current-posix-network) 'udp-open    family-host family-port)         kf k))
