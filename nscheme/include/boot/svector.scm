(define (svector . args)     (vector->svector (apply vector args)))
(define (svector-ref    x i) (vector-ref    (svector->vector x) i))
(define (svector-length x)   (vector-length (svector->vector x)))
