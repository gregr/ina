(define (make-platform-parameter key* platform.default)
  (case-lambda
    (()          (atree-ref* (list (current-platform) platform.default) key*))
    ((new thunk) (current-platform (atree-set (current-platform) key* new) thunk))))

(define platform.empty '())
(define platform.default
  (list (cons 'description '())
        (cons 'console     (list (cons 'input-port  empty-iport)
                                 (cons 'output-port full-oport)
                                 (cons 'output-port full-oport)))
        (cons 'time        (list (cons 'sleep-seconds-nanoseconds (lambda (s ns) (values)))
                                 (cons 'seconds-nanoseconds/type  (lambda (type) (lambda () (values 0 0))))))
        (cons 'primitive-evaluate (case-lambda
                                    (()                 '())
                                    ((type code kretry) (kretry '()))))))
(define current-input-port                (make-platform-parameter '(console input-port)             platform.default))
(define current-output-port               (make-platform-parameter '(console output-port)            platform.default))
(define current-error-port                (make-platform-parameter '(console error-port)             platform.default))
(define current-sleep-seconds-nanoseconds (make-platform-parameter '(time sleep-seconds-nanoseconds) platform.default))
(define current-seconds-nanoseconds/type  (make-platform-parameter '(time seconds-nanoseconds/type)  platform.default))
(define current-primitive-evaluate        (make-platform-parameter '(primitive-evaluate)             platform.default))
