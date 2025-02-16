(splicing-local
  ((define posix.default (vector #f #f #f #f #f))  ; TODO
   (define current-posix-parameter/index
     (current-platform-capability-parameter/index/type&default 'posix posix.default)))
  (define current-posix-argument*     (current-posix-parameter/index 0))
  (define current-posix-environment   (current-posix-parameter/index 1))
  (define current-posix-raw-process/k (current-posix-parameter/index 2))
  (define current-posix-filesystem    (current-posix-parameter/index 3))
  (define current-posix-network       (current-posix-parameter/index 4)))
