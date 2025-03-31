(define path.here (path-directory (car (current-posix-argument*))))
(define path.bootstrap/primitive.rkt (path-append path.here "primitive.rkt"))

(delete-file path.bootstrap/primitive.rkt)
(call/oport:file
  path.bootstrap/primitive.rkt 'create
  (lambda (out) (display (make-primitive.rkt-text) out)))
