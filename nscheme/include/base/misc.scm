(define (void . args) (values))
(define (not  x)      (if x #f #t))

(define (call-with-values produce consume) (apply/values consume (produce)))
(define call/values call-with-values)

(define (error . detail*) (panic 'error detail*))

(define (box        x) (make-mvector 1 x))
(define (unbox      b) (mvector-ref  b 0))
(define (set-box! b x) (mvector-set! b 0 x))

(define ((make-equal? super) a b)
  (let ? ((a a) (b b))
    (or (eqv? a b)
        (cond ((pair?   a) (and (pair? b) (? (car a) (car b)) (? (cdr a) (cdr b))))
              ((vector? a) (and (vector? b)
                                (= (vector-length a) (vector-length b))
                                (let ((end (vector-length a)))
                                  (let loop ((i 0))
                                    (or (= i end) (and (? (vector-ref a i) (vector-ref b i))
                                                       (loop (+ i 1))))))))
              (else        (super a b))))))
(define equal? (make-equal? (lambda (a b) #f)))

(define compose (case-lambda
                  ((f)        f)
                  ((f g . h*) (let loop ((f f) (g g) (h* h*))
                                (if (null? h*)
                                    (lambda x* (f (apply g x*)))
                                    (loop (lambda (x) (f (g x))) (car h*) (cdr h*)))))))
