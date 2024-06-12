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
            (let copy ((j 0))
              (if (< j len)
                  (begin (mbytevector-set! mbv (+ i j) (bytevector-ref bv j))
                         (copy (+ j 1)))
                  (loop (cdr x*) (+ i j)))))))))

(define (bytevector-append . x*) (bytevector-append* x*))
