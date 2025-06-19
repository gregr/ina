(define (list->vector x*) (apply vector x*))

(define vector->list
  (case-lambda
    ((v)             (vector->list v 0     (vector-length v)))
    ((v start)       (vector->list v start (- (vector-length v) start)))
    ((v start count) (let loop ((i (- (+ start count) 1)) (result '()))
                       (if (< i start)
                           result
                           (loop (- i 1) (cons (vector-ref v i) result)))))))

(define (vector-take x n) (vector->list x 0 (min (vector-length x) n)))

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
