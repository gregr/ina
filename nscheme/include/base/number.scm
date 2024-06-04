(define (exact? x) (rational? x))
(define (number? x) (exact? x))

(define (zero?     x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (integer-floor-div a b) (let-values (((d m) (integer-floor-divmod a b))) d))
(define (integer-floor-mod a b) (let-values (((d m) (integer-floor-divmod a b))) m))

(define (even? x) (= (integer-floor-mod x 2) 0))
(define (odd?  x) (= (integer-floor-mod x 2) 1))
(define (abs   x) (if (< x 0) (- x) x))

(define (max x . x*)
  (let loop ((x* x*) (current x))
    (cond ((null? x*) current)
          (else       (loop (cdr x*) (if (> (car x*) current) (car x*) current))))))

(define (min x . x*)
  (let loop ((x* x*) (current x))
    (cond ((null? x*) current)
          (else       (loop (cdr x*) (if (< (car x*) current) (car x*) current))))))

(define gcd
  (case-lambda
    (()         0)
    ((a)        (abs a))
    ((a b . x*) (define (gcd a b)
                  (let ((r (integer-floor-mod a b)))
                    (cond ((< 0 r) (gcd b r))
                          (else    b))))
                (let loop ((a (abs a)) (b (abs b)) (x* x*))
                  (let ((a (max a b)) (b (min a b)))
                    (let ((b (if (= b 0) a (gcd a b))))
                      (cond ((null? x*) b)
                            (else       (loop b (abs (car x*)) (cdr x*))))))))))

(define lcm
  (case-lambda
    (()         1)
    ((a)        (abs a))
    ((a b . x*) (if (= a 0)
                    0
                    (let loop ((a (abs a)) (b (abs b)) (x* x*))
                      (if (= b 0)
                          0
                          (let ((m (* a (/ b (gcd a b)))))
                            (cond ((null? x*) m)
                                  (else       (loop m (abs (car x*)) (cdr x*)))))))))))

(define (expt b p)
  (unless (integer? p) (error "not an integer" p))
  (let ((n (let loop ((b b) (p (abs p)))
             (cond ((< 1 p)
                    (let ((n (loop (* b b) (bitwise-arithmetic-shift-right p 1))))
                      (if (= (bitwise-and p 1) 1)
                          (* n b)
                          n)))
                   ((= 1 p) b)
                   (else    1)))))
    (if (< p 0) (/ 1 n) n)))

(define (floor n) (integer-floor-div (numerator n) (denominator n)))

(define (floor-log n b)
  (when (<= n 0) (error "not a positive number" n))
  (unless (and (integer? b) (< 1 b)) (error "not an integer greater than 1" b))
  (define (go n b)
    (if (<= b n)
        (let-values (((p b) (let loop ((b b) (p 1))
                              (let ((bb (* b b)))
                                (if (<= bb n)
                                    (let-values (((q m) (loop bb (+ p p))))
                                      (let ((mb (* m b)))
                                        (if (<= mb n)
                                            (values (+ q p) mb)
                                            (values q m))))
                                    (values p b))))))
          p)
        0))
  (if (< n 1)
      (- (+ (go (- (/ 1 n) 1) b) 1))
      (go n b)))
