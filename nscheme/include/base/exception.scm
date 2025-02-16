(define (make-exception   kind field*) (cons kind field*))
(define (exception-kind   exn)         (car exn))
(define (exception-field* exn)         (cdr exn))

(define (make-exception-kind superkind tag new-field-name*)
  (let* ((superkind (if superkind superkind '#()))
         (offset    (vector-length superkind))
         (mkind     (make-mvector (+ offset 2) 0)))
    (let loop ((i 0))
      (when (< i offset)
        (mvector-set! mkind i (vector-ref superkind i))
        (loop (+ i 1))))
    (mvector-set! mkind offset       tag)
    (mvector-set! mkind (+ offset 1) new-field-name*)
    (mvector->vector mkind)))
(define (exception-kind-offset          kind) (- (vector-length kind) 2))
(define (exception-kind-tag             kind) (vector-ref kind (exception-kind-offset kind)))
(define (exception-kind-new-field-name* kind) (vector-ref kind (+ (exception-kind-offset kind) 1)))
(define (exception-kind-? kind)
  (let ((offset (exception-kind-offset kind))
        (tag    (exception-kind-tag    kind)))
    (lambda (exn)
      (let ((kind (exception-kind exn)))
        (and (< offset (vector-length kind))
             (eqv? (vector-ref kind offset) tag))))))
(define (exception-kind-new-field-accessor* kind)
  (let* ((offset (let ((superkind-len (- (vector-length kind) 2)))
                   (let loop ((i 1) (offset 0))
                     (if (< i superkind-len)
                         (loop (+ i 2) (+ (vector-length (vector-ref kind i)) offset))
                         offset))))
         (field-count (+ (vector-length (vector-ref kind (- (vector-length kind) 1))) offset)))
    (let loop ((i offset))
      (if (< i field-count)
          (cons (lambda (exn) (vector-ref (exception-field* exn) i))
                (loop (+ i 1)))
          '()))))

(define (exception-field-name* exn)
  (let* ((kind (exception-kind exn))
         (len  (vector-length kind)))
    (let loop ((i 1) (new-field-name** '()) (field-name-count 0))
      (if (< i len)
          (let ((new-field-name* (vector-ref kind i)))
            (loop (+ i 2) (cons new-field-name* new-field-name**)
                  (+ field-name-count (vector-length new-field-name*))))
          (let ((mfield-name* (make-mvector field-name-count 0)))
            (let loop.outer ((i field-name-count) (new-field-name** new-field-name**))
              (if (null? new-field-name**)
                  (mvector->vector mfield-name*)
                  (let* ((new-field-name*      (car new-field-name**))
                         (new-field-name-count (vector-length new-field-name*))
                         (i                    (- i new-field-name-count)))
                    (let loop.inner ((j 0))
                      (if (< j new-field-name-count)
                          (begin (mvector-set! mfield-name* (+ i j) (vector-ref new-field-name* j))
                                 (loop.inner (+ j 1)))
                          (loop.outer i (cdr new-field-name**))))))))))))

(define (exception-pretty exn)
  (let* ((field*      (exception-field* exn))
         (field-name* (exception-field-name* exn))
         (field-count (vector-length field*)))
    (cons (list 'exception (exception-kind-tag (exception-kind exn)))
          (let loop ((i 0))
            (if (< i field-count)
                (cons (list (vector-ref field-name* i) (vector-ref field* i)) (loop (+ i 1)))
                '())))))

(define (make-exception-kind-etc . arg*)
  (let ((kind (apply make-exception-kind arg*)))
    (apply values kind (exception-kind-? kind) (exception-kind-new-field-accessor* kind))))

;;;;;;;;;;;;;
;;; Error ;;;
;;;;;;;;;;;;;
(define-values (error:kind error? error-description)
  (make-exception-kind-etc #f 'error '#(description)))
(define (make-error  desc) (make-exception error:kind (vector desc)))
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
(define-values (io-error:kind io-error? io-error-tag io-error-context)
  (make-exception-kind-etc error:kind 'io-error '#(tag context)))
(define (make-io-error  tag context) (make-exception io-error:kind (vector "IO error" tag context)))
(define (raise-io-error tag context) (raise (make-io-error tag context)))
