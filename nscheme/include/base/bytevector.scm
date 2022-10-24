(define (bytevector . bs) (mbytevector->bytevector (apply mbytevector bs)))
