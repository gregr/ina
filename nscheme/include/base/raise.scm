(define current-raise-handler* (make-parameter '()))
(define (raise-continuable x)
  (let loop ((h* (current-raise-handler*)))
    (unless (null? h*)
      (current-raise-handler*
        (cdr h*)
        (lambda ()
          ((car h*) x)
          (loop (cdr h*)))))))
(define (raise          x) (raise-continuable x) (panic 'raise x))
(define (raise/continue x) (with-restart:continue 'raise/continue (lambda () (raise x))))

(define (with-raise-handler handle thunk)
  (current-raise-handler* (cons handle (current-raise-handler*)) thunk))
(define (with-raise-handler* handle* thunk)
  (current-raise-handler* (append handle* (current-raise-handler*))) thunk)

(define (with-raise-handler:catch catch? handle thunk)
  (with-escape
    handle
    (lambda (escape)
      (with-raise-handler
        (lambda (exn) (when (catch? exn) (escape exn)))
        thunk))))
(define (with-raise-handler:catch* catch?handle* thunk)
  (with-escape
    (lambda (handle-exn) (handle-exn))
    (lambda (escape)
      (with-raise-handler*
        (map (lambda (catch?handle)
               (apply (lambda (catch? handle)
                        (lambda (exn) (when (catch? exn) (escape (lambda () (handle exn))))))
                      catch?handle))
             catch?handle*)
        thunk))))
