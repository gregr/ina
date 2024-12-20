(define (mvector . args)
  (let ((x (make-mvector (length args) 0)))
    (let loop ((i 0) (args args))
      (cond ((null? args) x)
            (else         (mvector-set! x i (car args))
                          (loop (+ i 1) (cdr args)))))))

(define mvector-fill!
  (let ((go (lambda (mv v start count)
              (nonnegative-integer? start)
              (nonnegative-integer? count)
              (let ((end (+ start count)))
                (unless (<= end (mvector-length mv))
                  (error "mvector-fill! range out of bounds" start count (mvector-length mv)))
                (range-for-each (lambda (i) (mvector-set! mv i v)) start end)))))
  (case-lambda
    ((mv v)             (go mv v 0     (mvector-length mv)))
    ((mv v start)       (go mv v start (- (mvector-length mv) start)))
    ((mv v start count) (go mv v start count)))))

(define mvector-copy!
  (local ((define (bounds?! dst start.dst len.src start.src count)
            (nonnegative-integer?! start.src)
            (nonnegative-integer?! start.dst)
            (nonnegative-integer?! count)
            (unless (<= (+ start.dst count) (mvector-length dst))
              (error "mvector-copy! destination range is out of bounds" start.dst count
                     (mvector-length dst)))
            (unless (<= (+ start.src count) len.src)
              (error "mvector-copy! source range is out of bounds" start.src count len.src)))
          (define (copy-forward! dst start.dst src start.src count)
            (range-for-each
              (lambda (i) (mvector-set! dst (+ start.dst i) (mvector-ref src (+ start.src i))))
              0 count))
          (define (copy-backward! dst start.dst src start.src count)
            (range-for-each
              (lambda (i) (mvector-set! dst (+ start.dst i) (mvector-ref src (+ start.src i))))
              (- count 1) -1 -1))
          (define (copy-vector! dst start.dst src start.src count)
            (range-for-each
              (lambda (i) (mvector-set! dst (+ start.dst i) (vector-ref src (+ start.src i))))
              0 count)))
    (lambda (dst start.dst src start.src count)
      (cond ((mvector? src) (bounds?! dst start.dst (mvector-length src) start.src count)
                            (if (and (eqv? dst src) (< start.src start.dst))
                                (copy-backward! dst start.dst src start.src count)
                                (copy-forward!  dst start.dst src start.src count)))
            ((vector? src)  (bounds?! dst start.dst (vector-length src) start.src count)
                            (copy-vector! dst start.dst src start.src count))
            (else           (error "not a vector or mvector" src))))))
