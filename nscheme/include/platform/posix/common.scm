(define package.posix.common
  (cons
    '(
      host-argument* host-environment host-make-raw-process/k
      current-filesystem
      tcp-listen/k tcp-connect/k udp-open/k)
    (list
      host-argument* host-environment host-make-raw-process/k
      current-filesystem
      tcp-listen/k tcp-connect/k udp-open/k)))
