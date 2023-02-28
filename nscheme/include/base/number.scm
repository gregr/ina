(define exact? rational?)
(define (inexact? x) (or (f32? x) (f64? x)))
(define (number?  x) (or (rational? x) (inexact? x)))

(define (integer?!  x) (has-type?! integer?  'integer?  x))
(define (rational?! x) (has-type?! rational? 'rational? x))

(define (=  a b) (rational?! a) (rational?! b) (eqv? a b))
(define (<= a b) (rational?! a) (rational?! b) (or (eqv? a b) (< a b)))
(define (>= a b) (rational?! a) (rational?! b) (or (eqv? a b) (< b a)))
(define (>  a b) (rational?! a) (rational?! b) (< b a))

(define (zero?     x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (integer-floor-div a b) (let-values (((d m) (integer-floor-divmod a b))) d))
(define (integer-floor-mod a b) (let-values (((d m) (integer-floor-divmod a b))) m))

(define (even? x)
  (integer?! x)
  (= (integer-floor-mod x 2) 0))

(define (odd? x)
  (integer?! x)
  (= (integer-floor-mod x 2) 1))

(define (abs x) (if (< x 0) (- x) x))

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
