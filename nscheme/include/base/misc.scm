(define (void . args) (values))
(define (not  x)      (if x #f #t))

(define (call-with-values produce consume) (apply/values consume (produce)))

(define (error . detail*) (panic 'error detail*))

(define (box        x) (let ((b (make-mvector 1 0))) (mvector-set! b 0 x) b))
(define (unbox      b) (mvector-ref  b 0))
(define (set-box! b x) (mvector-set! b 0 x))
