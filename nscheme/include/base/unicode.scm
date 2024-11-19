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
          (let ((b0 (bytevector-ref bv i)))
            (cond ((= (bitwise-and b0 #b10000000) #b00000000) (loop (+ i 1)))
                  ((= (bitwise-and b0 #b11100000) #b11000000)
                   (and (< #b11000001 b0) (< (+ i 1) end)
                        (extra-octet? (bytevector-ref bv (+ i 1)))
                        (loop (+ i 2))))
                  ((= (bitwise-and b0 #b11110000) #b11100000)
                   (and (< (+ i 2) end)
                        (let ((b1 (bytevector-ref bv (+ i 1)))
                              (b2 (bytevector-ref bv (+ i 2))))
                          (and (extra-octet? b1) (extra-octet? b2)
                               (let ((c (bitwise-ior
                                          (bitwise-arithmetic-shift-left (bitwise-and b0 #x0f) 12)
                                          (bitwise-arithmetic-shift-left (bitwise-and b1 #x3f) 6)
                                          (bitwise-and b2 #x3f))))
                                 (or (< #x7ff c #xd800) (< #xdfff c)))
                               (loop (+ i 3))))))
                  ((= (bitwise-and b0 #b11111000) #b11110000)
                   (and (< b0 #xb11110101) (< (+ i 3) end)
                        (let ((b1 (bytevector-ref bv (+ i 1)))
                              (b2 (bytevector-ref bv (+ i 2)))
                              (b3 (bytevector-ref bv (+ i 3))))
                          (and (extra-octet? b1) (extra-octet? b2) (extra-octet? b3)
                               (let ((c (bitwise-ior
                                          (bitwise-arithmetic-shift-left (bitwise-and b0 #x0f) 18)
                                          (bitwise-arithmetic-shift-left (bitwise-and b1 #x3f) 12)
                                          (bitwise-arithmetic-shift-left (bitwise-and b2 #x3f) 6)
                                          (bitwise-and b3 #x3f))))
                                 (< #xffff c #x110000))
                               (loop (+ i 4))))))
                  (else #f)))))))
