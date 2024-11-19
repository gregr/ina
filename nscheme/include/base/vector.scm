(define (list->vector x*) (apply vector x*))

(define (vector-take x n)
  (let ((len (min (vector-length x) n)))
    (let loop ((i 0))
      (if (< i len) (cons (vector-ref x i) (loop (+ i 1))) '()))))

(define (vector->list x) (vector-take x (vector-length x)))

(define (vector-append . x*) (list->vector (append* (map vector->list x*))))

(define (vector-for-each f x . x*) (apply for-each f (vector->list x) (map vector->list x*)))
(define (vector-map      f x . x*) (list->vector
                                     (apply map f (vector->list x) (map vector->list x*))))
