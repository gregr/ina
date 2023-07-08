(define (mvector . args)
  (let ((x (make-mvector (length args) 0)))
    (let loop ((i 0) (args args))
      (cond ((null? args) x)
            (else         (mvector-set! x i (car args))
                          (loop (+ i 1) (cdr args)))))))

(define (mvector-transform-range! mv start end f)
  ;; TODO: always requiring (= 0 start) ?  That's not very useful.
  (unless (and (= 0 start) (<= start end) (<= end (mvector-length mv)))
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

(define (box        x) (mvector x))
(define (unbox      b) (mvector-ref  b 0))
(define (set-box! b x) (mvector-set! b 0 x))
