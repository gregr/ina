(define (string-append . x*) (utf8->string (apply bytevector-append (map string->utf8 x*))))

(define (number->string  n) (utf8->string (number->utf8  n)))
(define (integer->string n) (utf8->string (integer->utf8 n)))
(define (number->utf8 n)
  (cond ((integer? n) (integer->utf8 n))
        (else         (error "TODO: number->utf8 for non-integers" n))))
(define (integer->utf8 n)
  (define char.zero 48)
  (define char.-    45)
  (define (nat->digit* n)
    (let loop ((n (abs n)) (d* '()))
      (let-values (((q r) (integer-floor-divmod n 10)))
        (let ((d* (cons (+ char.zero r) d*))) (if (< 0 q) (loop q d*) d*)))))
  (cond ((eq? n 0) #"0")
        ((<   n 0) (u8*->bytevector (cons char.- (nat->digit* (- n)))))
        (else      (u8*->bytevector (nat->digit* n)))))

;; TODO:
;string->number
