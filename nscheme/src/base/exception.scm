(define (make-exception-kind superkind tag field-name*) (vector tag field-name* superkind))
(define (exception-kind-tag         kind) (vector-ref kind 0))
(define (exception-kind-field-name* kind) (vector-ref kind 1))
(define (exception-kind-superkind   kind) (vector-ref kind 2))
(define (exception-kind-?           kind) (let ((tag (exception-kind-tag kind)))
                                            (lambda (ex) (and (assv tag ex) #t))))
(define (exception-kind-field-accessor kind field-name)
  (let ((tag (exception-kind-tag kind)))
    (lambda (ex) (alist-ref (alist-ref ex tag) field-name))))
(define (exception-kind-field-updater kind field-name)
  (let ((tag (exception-kind-tag kind)))
    (lambda (ex update)
      (alist-update ex tag (lambda (field*) (alist-update field* field-name update))))))
(define (exception-kind-field-setter kind field-name)
  (let ((update (exception-kind-field-updater kind field-name)))
    (lambda (ex x) (update ex (lambda (_) x)))))

(define (make-exception kind super . field*)
  (cons (cons (exception-kind-tag kind) (map cons (exception-kind-field-name* kind) field*))
        (or super '())))

;;;;;;;;;;;;;
;;; Error ;;;
;;;;;;;;;;;;;
(define error:kind        (make-exception-kind #f 'error '(description)))
(define error?            (exception-kind-?              error:kind))
(define error-description (exception-kind-field-accessor error:kind 'description))
(define (make-error  desc) (make-exception error:kind #f desc))
(define (raise-error desc) (raise (make-error desc)))

;;;;;;;;;;;;;;;;
;;; IO Error ;;;
;;;;;;;;;;;;;;;;
;;; Improper use of an IO operation, such as a port, iomemory, or platform device operation, will
;;; panic.  Proper use may still fail, indicated by two error description values:
;;; - a failure tag, typically a symbol, #f, or an integer code
;;;   - #f        ; failure is not categorized
;;;   - <integer> ; e.g., an errno value
;;;   - exists
;;;   - not-open
;;;   - no-space
;;;   - unsupported
;;; - a failure context, which is a list of detail frames tracing the failure across abstraction layers
;;; Depending on the operation variant, by convention these two values are either:
;;; - returned to a failure continuation for /k operations
;;; - raised as an io-error otherwise
(define io-error:kind    (make-exception-kind #f 'error '(tag context)))
(define io-error?        (exception-kind-?              io-error:kind))
(define io-error-tag     (exception-kind-field-accessor io-error:kind 'tag))
(define io-error-context (exception-kind-field-accessor io-error:kind 'context))
(define (make-io-error  tag context) (make-exception io-error:kind (make-error "IO error") tag context))
(define (raise-io-error tag context) (raise (make-io-error tag context)))
