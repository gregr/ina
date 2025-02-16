(splicing-local
  ((define posix.default
     (vector '() '()
             (lambda (in out err path arg* env kf k)
               (kf 'unsupported (list (vector '(unsupported posix-raw-process/k) path arg*))))
             (lambda (method . arg*) (error "no posix-filesystem" method))
             (lambda (method . arg*) (error "no posix-network" method))))
   (define current-posix-parameter/index
     (current-platform-capability-parameter/index/type&default 'posix posix.default)))
  (define current-posix-argument*     (current-posix-parameter/index 0))
  (define current-posix-environment   (current-posix-parameter/index 1))
  (define current-posix-raw-process/k (current-posix-parameter/index 2))
  (define current-posix-filesystem    (current-posix-parameter/index 3))
  (define current-posix-network       (current-posix-parameter/index 4)))
