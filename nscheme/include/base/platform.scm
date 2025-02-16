(define platform.empty '())
(define (platform-ref/default    p key default)        (alist-ref/default p key default))
(define (platform-remove         p key)                (alist-remove p key))
(define (platform-set            p key value)          (alist-set p key value))
(define (platform-update/default p key update default) (alist-update/default p key update default))

(define ((current-platform-capability-parameter/index/type&default type default) index)
  (case-lambda
    (() (vector-ref (platform-ref/default (current-platform) type default) index))
    ((new-value thunk)
     (current-platform (platform-update/default (current-platform) type
                                                (lambda (cap) (vector-set cap index new-value))
                                                default)
                       thunk))))

(define current-platform-name
  (case-lambda
    (()                (platform-ref/default (current-platform) 'name #f))
    ((new-value thunk) (current-platform (platform-set (current-platform) 'name new-value) thunk))))

(splicing-local
  ((define console.default (vector empty-iport full-oport full-oport))
   (define current-console-parameter/index
     (current-platform-capability-parameter/index/type&default 'console console.default)))
  (define current-input-port  (current-console-parameter/index 0))
  (define current-output-port (current-console-parameter/index 1))
  (define current-error-port  (current-console-parameter/index 2)))

(splicing-local
  ((define time.default (vector (lambda (s ns) (values)) (lambda (type) (lambda () (values 0 0)))))
   (define current-time-parameter/index
     (current-platform-capability-parameter/index/type&default 'time time.default)))
  (define current-sleep-seconds-nanoseconds (current-time-parameter/index 0))
  (define current-seconds-nanoseconds/type  (current-time-parameter/index 1)))
