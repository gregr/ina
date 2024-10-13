(define (list->bytevector x*) (apply bytevector x*))

(define (bytevector->list x)
  (let ((len (bytevector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (bytevector-ref x i) (loop (+ i 1))))))))

(define (bytevector-append* x*)
  (let ((mbv (make-mbytevector
               (let loop ((x* x*) (len 0))
                 (if (null? x*)
                     len
                     (loop (cdr x*) (+ (bytevector-length (car x*)) len))))
               0)))
    (let loop ((x* x*) (i 0))
      (if (null? x*)
          (mbytevector->bytevector mbv)
          (let* ((bv (car x*)) (len (bytevector-length bv)))
            (mbytevector-copy! bv 0 mbv i len)
            (loop (cdr x*) (+ i len)))))))

(define (bytevector-append . x*) (bytevector-append* x*))

(define (bytevector-join* separator x*)
  (if (null? x*)
      #""
      (let* ((len.sep (bytevector-length separator))
             (mbv (make-mbytevector
                    (let loop ((x (car x*)) (x* (cdr x*)) (final-size 0))
                      (let ((final-size (+ (bytevector-length x) final-size)))
                        (if (null? x*)
                            final-size
                            (loop (car x*) (cdr x*) (+ len.sep final-size)))))
                    0)))
        (let loop ((i 0) (x (car x*)) (x* (cdr x*)))
          (let ((len (bytevector-length x)))
            (mbytevector-copy! x 0 mbv i len)
            (if (null? x*)
                (mbytevector->bytevector mbv)
                (let ((i (+ i len)))
                  (mbytevector-copy! separator 0 mbv i len.sep)
                  (loop (+ i len.sep) (car x*) (cdr x*)))))))))

(define (bytevector-join separator . x*) (bytevector-join* separator x*))

(define (bytevector-split bv separator)
  (let ((len (bytevector-length bv)))
    (let loop-segment ((start 0))
      (let loop-byte ((i start))
        (define (make-segment) (let* ((len (- i start)) (segment (make-mbytevector len 0)))
                                 (mbytevector-copy! bv start segment 0 len)
                                 (mbytevector->bytevector segment)))
        (cond ((= i len)                           (list (make-segment)))
              ((= (bytevector-ref bv i) separator) (cons (make-segment) (loop-segment (+ i 1))))
              (else                                (loop-byte (+ i 1))))))))

(define (bytevector-rtrim bv b)
  (let ((len (bytevector-length bv)))
    (if (and (< 0 len) (= (bytevector-ref bv (- len 1)) b))
        (let ((new (make-mbytevector (- len 1) 0)))
          (mbytevector-copy! bv 0 new 0 (- len 1))
          (mbytevector->bytevector new))
        bv)))
