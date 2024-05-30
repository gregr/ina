(define (u8*->bytevector x*) (apply bytevector x*))

(define (bytevector->u8* x)
  (let ((len (bytevector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (bytevector-u8-ref x i) (loop (+ i 1))))))))

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
                  (begin (mbytevector-u8-set! mbv (+ i j) (bytevector-u8-ref bv j))
                         (copy (+ j 1)))
                  (loop (cdr x*) (+ i j)))))))))

(define (bytevector-append . x*) (bytevector-append* x*))
