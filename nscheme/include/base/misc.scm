(define (void . args) (values))
(define (not  x)      (if x #f #t))

(define (call-with-values produce consume) (apply/values consume (produce)))

(define (error . detail*) (panic 'error detail*))

(define (box        x) (let ((b (make-mvector 1 0))) (mvector-set! b 0 x) b))
(define (unbox      b) (mvector-ref  b 0))
(define (set-box! b x) (mvector-set! b 0 x))

(define (equal? a b)
  (or (eqv? a b)
      (cond ((pair?   a) (and (pair? b)
                              (equal? (car a) (car b))
                              (equal? (cdr a) (cdr b))))
            ((vector? a) (and (vector? b)
                              (= (vector-length a) (vector-length b))
                              (let ((end (vector-length a)))
                                (let loop ((i 0))
                                  (or (= i end)
                                      (and (equal? (vector-ref a i) (vector-ref b i))
                                           (loop (+ i 1))))))))
            (else        #f))))
