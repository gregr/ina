(define (void . args) (values))
(define (not  x)      (if x #f #t))
(define (boolean? x)  (or (eq? x #f) (eq? x #t)))
