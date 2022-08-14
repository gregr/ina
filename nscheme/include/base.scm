(define (void . args) (values))
(define (list . x*)   x*)
(define (not  x)      (if x #f #t))

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

;; TODO: define analogous operators for mbytevector

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
                (cond ((mvector?     src) (values mvector-ref     mvector-length))
                      ((vector?      src) (values vector-ref      vector-length))
                      ((mbytevector? src) (values mbytevector-ref mbytevector-length))
                      ((bytevector?  src) (values bytevector-ref  bytevector-length))
                      (else (error "invalid source for mvector-copy!" src)))))
    (unless (and (<= 0 start.src) (<= start.src end.src) (<= end.src (length src)))
      (error "invalid source range" 'length (length src) 'start start.src 'end end.src))
    (mvector-transform-range! mv start (+ start (- end.src start.src))
                              (lambda (i) (ref src (+ start.src (- i start)))))))

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
               (and (equal? (bytevector-ref a i) (bytevector-ref b i))
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

(define (member y x*) (memp (lambda (x) (equal? x y)) x*))

(define (rem1p ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cdr x*))
          (else         (cons (car x*) (loop (cdr x*)))))))

(define (remove1 y x*) (rem1p (lambda (x) (equal? x y)) x*))

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

(define (assoc key alist) (assp (lambda (k) (equal? k key)) alist))

(define (plist->alist kvs) (cond ((null? kvs) '())
                                 (else        (cons (cons (car kvs) (cadr kvs))
                                                    (plist->alist (cddr kvs))))))
