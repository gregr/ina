(define (void . args) (values))
(define (list . x*)   x*)
(define (not  x)      (if x #f #t))

(define (boolean? x)  (or (eq? x #f) (eq? x #t)))

;;;;;;;;;;;;;;
;;; Errors ;;;
;;;;;;;;;;;;;;

(define (make-error    kind details) (svector 'error kind details))
(define (error-kind    err)          (error?! err) (svector-ref err 1))
(define (error-details err)          (error?! err) (svector-ref err 2))
(define (error?        x)            (and (svector? x)
                                          (= (svector-length x) 3)
                                          (eq? (svector-ref x 0) 'error)))
(define (error?!       x)            (has-type?! error? 'error? x))

(define (raise-type-error expected value)
  (raise (make-error 'type (list 'expected expected 'actual value))))

(define (has-type?! type? expected value)
  (unless (type? value) (raise-type-error expected value)))

(define (raise-syntax-error description . stx*)
  (raise (make-error 'syntax (list 'description description 'syntax stx*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mutable vector and bytevector transformation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mvector-transform-range! mv start end f)
  (unless (and (<= 0 start) (<= start end) (<= end (mvector-length mv)))
    (error "invalid mvector range"
           'length (mvector-length mv) 'start start 'end end))
  (let loop ((i start)) (when (< i end)
                          (mvector-set! mv i (f i))
                          (loop (+ i 1)))))

(define (mvector-fill! mv v)
  (mvector-transform-range! mv 0 (mvector-length mv) (lambda (_) v)))

(define (mvector-copy! mv start src start.src end.src)
  (let-values (((ref length)
                (cond ((mvector? src) (values mvector-ref mvector-length))
                      ((vector?  src) (values vector-ref  vector-length))
                      (else (error "invalid source for mvector-copy!" src)))))
    (unless (and (<= 0 start.src) (<= start.src end.src) (<= end.src (length src)))
      (error "invalid source range" 'length (length src) 'start start.src 'end end.src))
    (mvector-transform-range! mv start (+ start (- end.src start.src))
                              (lambda (i) (ref src (+ start.src (- i start)))))))

(define (mbytevector-transform-range! m start end f)
  (unless (and (<= 0 start) (<= start end) (<= end (mbytevector-length m)))
    (error "invalid mvector range"
           'length (mbytevector-length m) 'start start 'end end))
  (let loop ((i start)) (when (< i end)
                          (mbytevector-u8-set! m i (f i))
                          (loop (+ i 1)))))

(define (mbytevector-fill! m v)
  (mbytevector-transform-range! m 0 (mbytevector-length m) (lambda (_) v)))

(define (mbytevector-copy! m start src start.src end.src)
  (let-values (((ref length)
                (cond ((mbytevector? src) (values mbytevector-u8-ref mbytevector-length))
                      ((bytevector?  src) (values bytevector-u8-ref  bytevector-length))
                      (else (error "invalid source for mbytevector-copy!" src)))))
    (unless (and (<= 0 start.src) (<= start.src end.src) (<= end.src (length src)))
      (error "invalid source range" 'length (length src) 'start start.src 'end end.src))
    (mbytevector-transform-range! m start (+ start (- end.src start.src))
                                  (lambda (i) (ref src (+ start.src (- i start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vectors and bytevectors ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (u8-list->bytevector x*) (apply bytevector x*))

(define (bytevector->u8-list x)
  (let ((len (bytevector-length x)))
    (let loop ((i 0))
      (cond ((= i len) '())
            (else      (cons (bytevector-u8-ref x i) (loop (+ i 1))))))))

(define (bytevector-append . x*) (u8-list->bytevector (apply append (map bytevector->u8-list x*))))

;;;;;;;;;;;;;;;;
;;; Equality ;;;
;;;;;;;;;;;;;;;;

(define (string?!     x) (has-type?! string?     'string?     x))
(define (vector?!     x) (has-type?! vector?     'vector?     x))
(define (bytevector?! x) (has-type?! bytevector? 'bytevector? x))

(define (vector=? a b)
  (vector?! a) (vector?! b)
  (and (= (vector-length a) (vector-length b))
       (let ((end (vector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (equal? (vector-ref a i) (vector-ref b i))
                    (loop (+ i 1))))))))

(define (bytevector=? a b)
  (bytevector?! a) (bytevector?! b)
  (and (= (bytevector-length a) (bytevector-length b))
       (let ((end (bytevector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (equal? (bytevector-u8-ref a i) (bytevector-u8-ref b i))
                    (loop (+ i 1))))))))

(define (string=? a b)
  (string?! a) (string?! b)
  (bytevector=? (string->utf8 a) (string->utf8 b)))

(define (equal? a b)
  (or (eqv? a b)
      (cond ((pair?       a) (and (pair? b)
                                  (equal? (car a) (car b))
                                  (equal? (cdr a) (cdr b))))
            ((vector?     a) (and (vector?     b) (vector=?     a b)))
            ((bytevector? a) (and (bytevector? b) (bytevector=? a b)))
            ((string?     a) (and (string?     b) (string=? a b)))
            (else            #f))))

;;;;;;;;;;;;;
;;; Lists ;;;
;;;;;;;;;;;;;

(splicing-local
  ((define (append2 x* y)
     (cond ((null? x*) y)
           (else       (cons (car x*) (append2 (cdr x*) y))))))
  (define append
    (case-lambda
      (()          '())
      ((x* . rest) (let loop ((x* x*) (rest rest))
                     (cond ((null? rest) x*)
                           (else         (append2 x* (loop (car rest) (cdr rest))))))))))

(define (improper-list->list x*)
  (cond ((null? x*) '())
        ((pair? x*) (cons (car x*) (improper-list->list (cdr x*))))
        (else       (list x*))))

(define (improper-list-map f x*)
  (let loop ((x* x*))
    (cond ((null? x*) '())
          ((pair? x*) (cons (f (car x*)) (loop (cdr x*))))
          (else       (f x*)))))

(define (memp ? x*)
  (let loop ((x* x*))
    (and (not (null? x*))
         (cond ((? (car x*)) x*)
               (else         (loop (cdr x*)))))))

(define (mem/= =? y x*) (memp (lambda (x) (=? x y)) x*))

(define (memq   y x*) (mem/= eq?    y x*))
(define (memv   y x*) (mem/= eqv?   y x*))
(define (member y x*) (mem/= equal? y x*))

(define (rem1p ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cdr x*))
          (else         (cons (car x*) (loop (cdr x*)))))))

(define (rem/= =? y x*) (rem1p (lambda (x) (=? x y)) x*))

(define (remq1   y x*) (rem/= eq?    y x*))
(define (remv1   y x*) (rem/= eqv?   y x*))
(define (remove1 y x*) (rem/= equal? y x*))

(define (iota n)
  (unless (and (integer? n) (<= 0 n)) (error "not a nonnegative integer" n))
  (let loop ((i 0))
    (cond ((= i n) '())
          (else    (cons i (loop (+ i 1)))))))

(define (assp ? alist)
  (let loop ((alist alist))
    (and (not (null? alist))
         (let ((kv (car alist)))
           (cond ((? (car kv)) kv)
                 (else         (loop (cdr alist))))))))

(define (assoc/= =? key alist) (assp (lambda (k) (=? k key)) alist))

(define (assq  key alist) (assoc/= eq?    key alist))
(define (assv  key alist) (assoc/= eqv?   key alist))
(define (assoc key alist) (assoc/= equal? key alist))

(define (plist->alist kvs) (cond ((null? kvs) '())
                                 (else        (cons (cons (car kvs) (cadr kvs))
                                                    (plist->alist (cddr kvs))))))

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (length x*)
  (let loop ((x* x*) (l 0))
    (cond ((null? x*) l)
          (else (loop (cdr x*) (+ l 1))))))

(define (list-tail x* i)
  (unless (<= 0 i) (error "not a nonnegative integer" i))
  (let loop ((x* x*) (i i))
    (cond ((= i 0) x*)
          (else    (loop (cdr x*) (- i 1))))))

(define (list-ref x* i) (car (list-tail x* i)))

(define (cons* x . x*)
  (let loop ((x x) (x* x*))
    (cond ((null? x*) x)
          (else       (cons x (loop (car x*) (cdr x*)))))))

(define (reverse x*)
  (let loop ((x* x*) (acc '()))
    (cond ((null? x*) acc)
          (else       (loop (cdr x*) (cons (car x*) acc))))))

(define (filter ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cons (car x*) (loop (cdr x*))))
          (else         (loop (cdr x*))))))

(define (filter-not ? x*) (filter (lambda (x) (not (? x))) x*))
(define (true* x*)        (filter (lambda (x) x)           x*))
(define (null*? x*)       (let loop ((x* x*))
                            (or (null? x*) (and (null? (car x*)) (loop (cdr x*))))))

(splicing-local
  ((define (map1 f x*)
     (let loop ((x* x*))
       (cond ((null? x*) '())
             (else       (cons (f (car x*)) (loop (cdr x*)))))))
   (define (andmap1 f x*)
     (or (null? x*)
         (let loop ((x (car x*)) (x* (cdr x*)))
           (cond ((null? x*) (f x))
                 (else       (and (f x) (loop (car x*) (cdr x*)))))))))

  (define map
    (case-lambda
      ((f x*)       (map1 f x*))
      ((f y* . y**) (let loop ((x* y*) (x** y**))
                      (cond ((null? x*) (unless (andmap1 null? x**)
                                          (error "lists of different length"
                                                 (cons (length y*) (map1 length y**))))
                                        '())
                            (else (cons (apply f (car x*) (map1 car x**))
                                        (loop (cdr x*) (map1 cdr x**)))))))))

  (define for-each
    (case-lambda
      ((f x*)       (let loop ((x* x*)) (unless (null? x*) (f (car x*)) (loop (cdr x*)))))
      ((f y* . y**) (let loop ((x* y*) (x** y**))
                      (cond ((null? x*) (unless (andmap1 null? x**)
                                          (error "lists of different length"
                                                 (cons (length y*) (map1 length y**)))))
                            (else       (apply f (car x*) (map1 car x**))
                                        (loop (cdr x*) (map1 cdr x**))))))))

  (define andmap
    (case-lambda
      ((f x*)       (andmap1 f x*))
      ((f y* . y**) (cond ((null? y*) (unless (andmap1 null? y**)
                                        (error "lists of different length"
                                               (cons (length y*) (map1 length y**))))
                                      #t)
                          (else (let loop ((x (car y*)) (x* (cdr y*)) (x** y**))
                                  (cond ((null? x*) (unless (andmap1 null? (map1 cdr x**))
                                                      (error "lists of different length"
                                                             (cons (length y*) (map1 length y**))))
                                                    (apply f x (map1 car x**)))
                                        (else (and (apply f x (map1 car x**))
                                                   (loop (car x*) (cdr x*) (map1 cdr x**)))))))))))

  (define ormap
    (case-lambda
      ((f x*)       (and (not (null? x*))
                         (let loop ((x (car x*)) (x* (cdr x*)))
                           (cond ((null? x*) (f x))
                                 (else (or (f x) (loop (car x*) (cdr x*))))))))
      ((f y* . y**) (cond ((null? y*) (unless (andmap1 null? y**)
                                        (error "lists of different length"
                                               (cons (length y*) (map1 length y**))))
                                      #f)
                          (else (let loop ((x (car y*)) (x* (cdr y*)) (x** y**))
                                  (cond ((null? x*) (unless (andmap1 null? (map1 cdr x**))
                                                      (error "lists of different length"
                                                             (cons (length y*) (map1 length y**))))
                                                    (apply f x (map1 car x**)))
                                        (else (or (apply f x (map1 car x**))
                                                  (loop (car x*) (cdr x*) (map1 cdr x**))))))))))))

;;;;;;;;;;;;;;;
;;; Numbers ;;;
;;;;;;;;;;;;;;;

(define (integer?! x) (has-type?! integer? 'integer? x))

(define (zero?     x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (even? x)
  (integer?! x)
  (= (remainder x 2) 0))

(define (odd? x)
  (integer?! x)
  (= (remainder x 2) 1))

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
                  (let ((r (remainder a b)))
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

;;;;;;;;;;;;;;;
;;; Strings ;;;
;;;;;;;;;;;;;;;

(define (string-append . x*) (utf8->string (apply bytevector-append (map string->utf8 x*))))
