(define (void . args) (values))
(define (not  x)      (if x #f #t))

(define (call-with-values produce consume) (apply/values consume (produce)))
