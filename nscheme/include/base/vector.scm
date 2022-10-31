(define (vector . args) (mvector->vector (apply mvector args)))

(define (list->vector x*) (apply vector x*))

(define (vector->list x)
  (let ((len (vector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (vector-ref x i) (loop (+ i 1))))))))

(define (vector-append . x*) (list->vector (apply append (map vector->list x*))))

(define (vector-for-each f x . x*) (apply for-each f (vector->list x) (map vector->list x*)))
(define (vector-map      f x . x*) (list->vector
                                     (apply map f (vector->list x) (map vector->list x*))))
