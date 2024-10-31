(define (make-rtd identity? name parent final? field* extra)
  (when (and parent (rtd-final? parent))
    (error "rtd parent is final" name parent final? field*))
  (let ((ancestor* (let loop ((parent parent) (a* '()))
                     (cond (parent (loop (rtd-parent parent) (cons parent a*)))
                           (else   a*)))))
    (apply vector (+ (if parent (rtd-record-length parent) 0) (vector-length field*))
           (or identity? (make-mvector 0 0)) name final? field* extra ancestor*)))

(define rtd-base-length 6)
(define (rtd-record-length  rtd) (vector-ref    rtd 0))
(define (rtd-name           rtd) (vector-ref    rtd 2))
(define (rtd-final?         rtd) (vector-ref    rtd 3))
(define (rtd-field*         rtd) (vector-ref    rtd 4))
(define (rtd-extra          rtd) (vector-ref    rtd 5))
(define (rtd-ancestor-index rtd) (vector-length rtd))
(define (rtd-parent         rtd) (let ((len (vector-length rtd)))
                                   (and (< rtd-base-length len) (vector-ref rtd (- len 1)))))
(define (rtd=?              a b) (or (eq? a b) (eq? (vector-ref a 1) (vector-ref b 1))))

(define ((record-constructor rtd) . arg*) (apply record rtd arg*))

(define (record-predicate rtd)
  (if (rtd-final? rtd)
      (lambda (x) (and (record? x) (rtd=? (record-type-descriptor x) rtd)))
      (let ((idx (rtd-ancestor-index rtd)))
        (lambda (x) (and (record? x)
                         (let ((rtd.x (record-type-descriptor x)))
                           (or (rtd=? rtd.x rtd)
                               (and (< idx (vector-length rtd.x))
                                    (rtd=? (vector-ref rtd.x idx) rtd)))))))))

(define (record-field-position-accessor rtd i)
  (let ((count.field* (vector-length (rtd-field* rtd))))
    (unless (and (nonnegative-integer? i) (< i count.field*)) (error "not a field position" rtd i))
    (let ((? (record-predicate rtd))
          (i (+ (- (rtd-record-length rtd) count.field*) i)))
      (lambda (r)
        (unless (? r) (error "not the right kind of record" rtd r))
        (record-ref r i)))))

(define (record-field-name-accessor rtd name)
  (record-field-position-accessor rtd (record-field-name->position rtd name)))

(define (record-field-name->position rtd name)
  (let* ((field*       (rtd-field* rtd))
         (count.field* (vector-length field*)))
    (let loop ((i 0))
      (cond ((<= count.field* i)               (error "not a field name" rtd name))
            ((eqv? (vector-ref field* i) name) i)
            (else                              (loop (+ i 1)))))))
