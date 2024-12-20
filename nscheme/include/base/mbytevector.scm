(define (mbytevector . args)
  (let ((x (make-mbytevector (length args) 0)))
    (let loop ((i 0) (args args))
      (cond ((null? args) x)
            (else         (mbytevector-set! x i (car args))
                          (loop (+ i 1) (cdr args)))))))

(define mbytevector-fill!
  (let ((go (lambda (mbv v start count)
              (nonnegative-integer? start)
              (nonnegative-integer? count)
              (let ((end (+ start count)))
                (unless (<= end (mbytevector-length mbv))
                  (error "mbytevector-fill! range out of bounds" start count
                         (mbytevector-length mbv)))
                (range-for-each (lambda (i) (mbytevector-set! mbv i v)) start end)))))
  (case-lambda
    ((mbv v)             (go mbv v 0     (mbytevector-length mbv)))
    ((mbv v start)       (go mbv v start (- (mbytevector-length mbv) start)))
    ((mbv v start count) (go mbv v start count)))))

(define mbytevector-copy!
  (local
    ((define (bounds?! dst start.dst len.src start.src count)
       (nonnegative-integer?! start.src)
       (nonnegative-integer?! start.dst)
       (nonnegative-integer?! count)
       (unless (<= (+ start.dst count) (mbytevector-length dst))
         (error "mbytevector-copy! destination range is out of bounds" start.dst count
                (mbytevector-length dst)))
       (unless (<= (+ start.src count) len.src)
         (error "mbytevector-copy! source range is out of bounds" start.src count len.src)))
     (define (copy-forward! dst start.dst src start.src count)
       (range-for-each
         (lambda (i) (mbytevector-set! dst (+ start.dst i) (mbytevector-ref src (+ start.src i))))
         0 count))
     (define (copy-backward! dst start.dst src start.src count)
       (range-for-each
         (lambda (i) (mbytevector-set! dst (+ start.dst i) (mbytevector-ref src (+ start.src i))))
         (- count 1) -1 -1))
     (define (copy-bytevector! dst start.dst src start.src count)
       (range-for-each
         (lambda (i) (mbytevector-set! dst (+ start.dst i) (bytevector-ref src (+ start.src i))))
         0 count)))
    (lambda (dst start.dst src start.src count)
      (cond ((mbytevector? src) (bounds?! dst start.dst (mbytevector-length src) start.src count)
                                (if (and (eqv? dst src) (< start.src start.dst))
                                    (copy-backward! dst start.dst src start.src count)
                                    (copy-forward!  dst start.dst src start.src count)))
            ((bytevector? src)  (bounds?! dst start.dst (bytevector-length src) start.src count)
                                (copy-bytevector! dst start.dst src start.src count))
            (else               (error "not a bytevector or mbytevector" src))))))
