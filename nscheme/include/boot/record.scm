(define (make-rtd name parent sealed? field*)
  (when (and parent (rtd-sealed? parent))
    (violation "sealed parent rtd" (list 'make-rtd name parent sealed? field*)))
  (vector (+ (if parent (rtd-length parent) 0) (vector-length field*)) name parent sealed? field*))

(define (rtd-length  rtd) (vector-ref rtd 0))
(define (rtd-name    rtd) (vector-ref rtd 1))
(define (rtd-parent  rtd) (vector-ref rtd 2))
(define (rtd-sealed? rtd) (vector-ref rtd 3))
(define (rtd-field*  rtd) (vector-ref rtd 4))

(define (record-constructor rtd)
  (let ((len (rtd-length rtd)))
    (lambda arg*
      (unless (= (length arg*) len)
        (procedure-arity-violation (list 'record-constructor rtd) len (length arg*)))
      (apply record rtd arg*))))

(define (record-predicate rtd)
  (if (rtd-sealed? rtd)
      (lambda (x) (and (record? x) (eq? (record-type-descriptor x) rtd)))
      ;; TODO: maybe lay out all ancestors in rtd for constant time testing
      (lambda (x) (and (record? x) (let loop ((rtd.x (record-type-descriptor x)))
                                     (or (eq? rtd.x rtd)
                                         (let ((rtd.x (rtd-parent rtd.x)))
                                           (and rtd.x (loop rtd.x)))))))))

(define (record-accessor rtd i)
  (let ((count.field* (vector-length (rtd-field* rtd))))
    (unless (and (integer? i) (<= 0 i) (< i count.field*))
      (violation "not a field accessor index" (list 'record-accessor rtd i)))
    (let ((? (record-predicate rtd))
          (i (+ (- (rtd-length rtd) count.field*) i)))
      (lambda (r) (has-type?! ? rtd r) (record-ref r i)))))
