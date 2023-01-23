(define (bytevector . bs) (mbytevector->bytevector (apply mbytevector bs)))

(define (u8*->bytevector x*) (apply bytevector x*))

(define (bytevector->u8* x)
  (let ((len (bytevector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (bytevector-u8-ref x i) (loop (+ i 1))))))))

(define (bytevector-append . x*) (u8*->bytevector (apply append (map bytevector->u8* x*))))
