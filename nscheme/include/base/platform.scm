(define platform.empty '())
(define (platform-ref/default    p key default)        (alist-ref/default p key default))
(define (platform-remove         p key)                (alist-remove p key))
(define (platform-set            p key value)          (alist-set p key value))
(define (platform-update/default p key update default) (alist-update/default p key update default))

(define (make-platform-field name default)
  (case-lambda
    ((p)        (platform-ref/default    p name        default))
    ((p update) (platform-update/default p name update default))))
(define (current-platform-field-parameter field subfield)
  (case-lambda
    (()          (subfield (field (current-platform))))
    ((new thunk) (field (current-platform) (lambda (entry) (subfield entry new))))))

(define platform-immediate-field (case-lambda
                                   ((x)     x)
                                   ((x new) new)))
(define make-platform-table vector)
(define (make-platform-table-field index)
  (case-lambda
    ((pt)     (vector-ref pt index))
    ((pt new) (vector-set pt index new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typical platform fields ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-console-table input-port output-port error-port)
  (make-platform-table input-port output-port error-port))
(define console-table-input-port  (make-platform-table-field 0))
(define console-table-output-port (make-platform-table-field 1))
(define console-table-error-port  (make-platform-table-field 2))
(define console-table.default (make-platform-table empty-iport full-oport full-oport))

(define (make-time-table sleep-seconds-nanoseconds seconds-nanoseconds/type)
  (vector sleep-seconds-nanoseconds seconds-nanoseconds/type))
(define time-table-sleep-seconds-nanoseconds (make-platform-table-field 0))
(define time-table-seconds-nanoseconds/type  (make-platform-table-field 1))
(define time-table.default (vector (lambda (s ns) (values)) (lambda (type) (lambda () (values 0 0)))))

(define platform-description              (make-platform-field 'description '()))
(define platform-time                     (make-platform-field 'time        time-table.default))
(define platform-console                  (make-platform-field 'console     console-table.default))
(define current-platform-description      (current-platform-field-parameter platform-description platform-immediate-field))
(define current-input-port                (current-platform-field-parameter platform-console console-table-input-port))
(define current-output-port               (current-platform-field-parameter platform-console console-table-output-port))
(define current-error-port                (current-platform-field-parameter platform-console console-table-error-port))
(define current-sleep-seconds-nanoseconds (current-platform-field-parameter platform-time time-table-sleep-seconds-nanoseconds))
(define current-seconds-nanoseconds/type  (current-platform-field-parameter platform-time time-table-seconds-nanoseconds/type))
