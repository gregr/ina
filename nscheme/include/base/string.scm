;;;;;;;;;;;;;;;;;;;;;;
;;; UTF-8 encoding ;;;
;;;;;;;;;;;;;;;;;;;;;;
;; U+000000 - U+00007f encoded as octets: 0xxxxxxx
;; U+000080 - U+0007ff encoded as octets: 110xxxxx 10xxxxxx
;; U+000800 - U+00ffff encoded as octets: 1110xxxx 10xxxxxx 10xxxxxx
;; U+010000 - U+10ffff encoded as octets: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
;; Invalid ranges: U+d800 - U+dfff and above U+10ffff
;; With correct encoding, which uses the minimum number of octets, these octets will never appear:
;; c0: 11000000
;; c1: 11000001
;; f5-ff: 11110101 or larger
(define (utf8? bv)
  (define (extra-octet? b) (= (bitwise-and b #b11000000) #b10000000))
  (let ((end (bytevector-length bv)))
    (let loop ((i 0))
      (or (= i end)
          (let ((b0 (bytevector-u8-ref bv i)))
            (cond ((= (bitwise-and b0 #b10000000) #b00000000) (loop (+ i 1)))
                  ((= (bitwise-and b0 #b11100000) #b11000000)
                   (and (< #b11000001 b0) (< (+ i 1) end)
                        (extra-octet? (bytevector-u8-ref bv (+ i 1)))
                        (loop (+ i 2))))
                  ((= (bitwise-and b0 #b11110000) #b11100000)
                   (and (< (+ i 2) end)
                        (let ((b1 (bytevector-u8-ref bv (+ i 1)))
                              (b2 (bytevector-u8-ref bv (+ i 2))))
                          (and (extra-octet? b1) (extra-octet? b2)
                               (let ((c (bitwise-ior
                                          (bitwise-arithmetic-shift-left (bitwise-and b0 #x0f) 12)
                                          (bitwise-arithmetic-shift-left (bitwise-and b1 #x3f) 6)
                                          (bitwise-and b2 #x3f))))
                                 (or (< #x7ff c #xd800) (< #xdfff c)))
                               (loop (+ i 3))))))
                  ((= (bitwise-and b0 #b11111000) #b11110000)
                   (and (< b0 #xb11110101) (< (+ i 3) end)
                        (let ((b1 (bytevector-u8-ref bv (+ i 1)))
                              (b2 (bytevector-u8-ref bv (+ i 2)))
                              (b3 (bytevector-u8-ref bv (+ i 3))))
                          (and (extra-octet? b1) (extra-octet? b2) (extra-octet? b3)
                               (let ((c (bitwise-ior
                                          (bitwise-arithmetic-shift-left (bitwise-and b0 #x0f) 18)
                                          (bitwise-arithmetic-shift-left (bitwise-and b1 #x3f) 12)
                                          (bitwise-arithmetic-shift-left (bitwise-and b2 #x3f) 6)
                                          (bitwise-and b3 #x3f))))
                                 (< #xffff c #x110000))
                               (loop (+ i 4))))))
                  (else #f)))))))

(define integer->utf8
  (let* ((byte:- 45)
         (byte:0 48)
         (byte:a 97)
         (digit (lambda (n) (if (< n 10) (+ n byte:0) (+ (- n 10) byte:a))))
         (go (lambda (n radix)
               (cond ((< n 0) (let* ((digit-count (+ (floor-log (- n) radix) 2))
                                     (mbv         (make-mbytevector digit-count 0)))
                                (mbytevector-u8-set! mbv 0 byte:-)
                                (let loop ((i (- digit-count 1)) (n (- n)))
                                  (if (< 0 i)
                                      (let-values (((q r) (integer-floor-divmod n radix)))
                                        (mbytevector-u8-set! mbv i (digit r))
                                        (loop (- i 1) q))
                                      (mbytevector->bytevector mbv)))))
                     ((< 0 n) (let* ((digit-count (+ (floor-log n radix) 1))
                                     (mbv         (make-mbytevector digit-count 0)))
                                (let loop ((i (- digit-count 1)) (n n))
                                  (if (<= 0 i)
                                      (let-values (((q r) (integer-floor-divmod n radix)))
                                        (mbytevector-u8-set! mbv i (digit r))
                                        (loop (- i 1) q))
                                      (mbytevector->bytevector mbv)))))
                     (else #"0")))))
    (case-lambda
      ((n) (go n 10))
      ((n radix)
       (unless (and (integer? radix) (<= 2 radix 16)) (error "unsupported radix" radix))
       (go n radix)))))

(define number->utf8
  (let ((go (lambda (n radix)
              (cond ((integer? n) (integer->utf8 n radix))
                    (else         (error "TODO: number->utf8 for non-integers" n radix))))))
    (case-lambda
      ((n)       (go n 10))
      ((n radix) (go n radix)))))

(define (utf8->number bv) (error "TODO: utf8->number" bv))

(define (string->utf8 s) (string->bytevector s))
(define (utf8->string bv)
  (unless (utf8? bv) (error "not UTF-8 encoded" bv))
  (bytevector->string bv))

(define (number->string  . x*) (utf8->string (apply number->utf8  x*)))
(define (integer->string . x*) (utf8->string (apply integer->utf8 x*)))
(define (string->number  . x*) (utf8->number (apply string->utf8  x*)))

(define (string-append* x*) (bytevector->string (bytevector-append* (map string->bytevector x*))))
(define (string-append . x*) (string-append* x*))

(define (make-local-gensym)
  (mlet ((count -1))
    (lambda (str)
      (set! count (+ count 1))
      (string->symbol (string-append str "." (number->string count))))))
