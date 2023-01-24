(define (vector=? a b)
  (and (= (vector-length a) (vector-length b))
       (let ((end (vector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (equal? (vector-ref a i) (vector-ref b i))
                    (loop (+ i 1))))))))

(define (bytevector=? a b)
  (and (= (bytevector-length a) (bytevector-length b))
       (let ((end (bytevector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (= (bytevector-u8-ref a i) (bytevector-u8-ref b i))
                    (loop (+ i 1))))))))

;; TODO: variadic?
(define (string=? a b) (bytevector=? (string->bytevector a) (string->bytevector b)))

;; TODO: string<? string<=? string-compare

(define (equal? a b)
  (or (eqv? a b)
      (cond ((pair?       a) (and (pair? b)
                                  (equal? (car a) (car b))
                                  (equal? (cdr a) (cdr b))))
            ((vector?     a) (and (vector?     b) (vector=?     a b)))
            ((bytevector? a) (and (bytevector? b) (bytevector=? a b)))
            ((string?     a) (and (string?     b) (string=? a b)))
            (else            #f))))
