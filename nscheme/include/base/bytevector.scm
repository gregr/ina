(define (bytevector . bs) (mbytevector->bytevector (apply mbytevector bs)))

(define (b8*->bytevector x*) (apply bytevector x*))

(define (bytevector->b8* x)
  (let ((len (bytevector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (bytevector-b8-ref x i) (loop (+ i 1))))))))

(define (bytevector-append . x*) (b8*->bytevector (apply append (map bytevector->b8* x*))))
