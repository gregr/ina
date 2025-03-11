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

(define (vector-set x i v)
  (let ((len (vector-length x)))
    (nonnegative-integer? i)
    (unless (< i len) (mistake 'vector-set "index out of bounds" 'index i 'length len))
    (let ((new (make-mvector len 0)))
      (mvector-copy! new 0 x 0 len)
      (mvector-set! new i v)
      (mvector->vector new))))
(define (vector-update x i update) (vector-set x i (update (vector-ref x i))))
