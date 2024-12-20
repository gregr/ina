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

(define (mvector-copy! src start.src dst start.dst count)
  (nonnegative-integer?! start.src)
  (nonnegative-integer?! start.dst)
  (nonnegative-integer?! count)
  (unless (<= (+ start.dst count) (mvector-length dst))
    (error "mvector-copy! destination range is out of bounds" start.dst count
           (mvector-length dst)))
  (define (go ref len.src)
    (unless (<= (+ start.src count) len.src)
      (error "mvector-copy! source range is out of bounds" start.src count len.src))
    (if (and (eqv? src dst) (< start.src start.dst))
        (range-for-each (lambda (i) (mvector-set! dst (+ start.dst i) (ref src (+ start.src i))))
                        (- count 1) -1 -1)
        (range-for-each (lambda (i) (mvector-set! dst (+ start.dst i) (ref src (+ start.src i))))
                        0 count)))
  (cond ((mvector? src) (go mvector-ref (mvector-length src)))
        ((vector? src)  (go vector-ref (vector-length src)))
        (else           (error "not a vector or mvector" src))))
