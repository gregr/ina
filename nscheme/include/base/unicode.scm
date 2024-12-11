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
(define (utf8-decode-width b0) (cond ((= (bitwise-and b0 #b10000000) #b00000000) 1)
                                     ((= (bitwise-and b0 #b11100000) #b11000000) 2)
                                     ((= (bitwise-and b0 #b11110000) #b11100000) 3)
                                     ((= (bitwise-and b0 #b11111000) #b11110000) 4)
                                     (else (error "not a utf8 starting byte" b0))))
(splicing-local
  ((define (utf8-continuation b)
     (unless (= (bitwise-and b #b11000000) #b10000000) (error "not a utf8 continuation byte" b))
     (bitwise-and b #x3f)))
  (define (utf8-decode-2 b0 b1)
    (unless (< #b11000001 b0) (error "not a utf8 2-byte sequence" b0 b1))
    (bitwise-ior (bitwise-asl (bitwise-and b0 #x1f) 6) (utf8-continuation b1)))
  (define (utf8-decode-3 b0 b1 b2)
    (let ((c (bitwise-ior (bitwise-asl (bitwise-and b0 #x0f)  12)
                          (bitwise-asl (utf8-continuation b1)  6)
                          (utf8-continuation b2))))
      (unless (or (< #x7ff c #xd800) (< #xdfff c)) (error "not a utf8 3-byte sequence" b0 b1 b2))
      c))
  (define (utf8-decode-4 b0 b1 b2 b3)
    (let ((c (bitwise-ior (bitwise-asl (bitwise-and b0 #x0f)  18)
                          (bitwise-asl (utf8-continuation b1) 12)
                          (bitwise-asl (utf8-continuation b2)  6)
                          (utf8-continuation b3))))
      (unless (< #xffff c #x110000) (error "not a utf8 4-byte sequence" b0 b1 b2 b3))
      c)))

(define (utf8-ref/b0&width bv i b0 width)
  (case width
    ((1) b0)
    ((2) (unless (< (+ i 1) (bytevector-length bv))
           (error "missing 2-byte utf8 continuation" bv i (bytevector-length bv)))
         (utf8-decode-2 b0 (bytevector-ref bv (+ i 1))))
    ((3) (unless (< (+ i 2) (bytevector-length bv))
           (error "missing 3-byte utf8 continuation" bv i (bytevector-length bv)))
         (utf8-decode-3 b0 (bytevector-ref bv (+ i 1)) (bytevector-ref bv (+ i 2))))
    ((4) (unless (< (+ i 3) (bytevector-length bv))
           (error "missing 4-byte utf8 continuation" bv i (bytevector-length bv)))
         (utf8-decode-4 b0 (bytevector-ref bv (+ i 1)) (bytevector-ref bv (+ i 2))
                         (bytevector-ref bv (+ i 3))))
    (else (error "not a utf8 width" bv i b0 width))))
(define (utf8?! bv)      (let ((len (bytevector-length bv)))
                           (let loop ((i 0)) (when (< i len) (let* ((b0    (bytevector-ref bv i))
                                                                    (width (utf8-decode-width b0)))
                                                               (utf8-ref/b0&width bv i b0 width)
                                                               (loop (+ i width)))))))
(define (utf8-ref bv i)  (let* ((b0 (bytevector-ref bv i)) (width (utf8-decode-width b0)))
                           (utf8-ref/b0&width bv i b0 width)))
(define (utf8-next bv i) (+ (utf8-decode-width (bytevector-ref bv i)) i))
(define (utf8-length bv) (let ((len (bytevector-length bv)))
                           (let loop ((i 0) (count 0))
                             (if (< i len) (loop (utf8-next bv i) (+ count 1)) count))))
(define (utf8-index bv n) (let ((len (bytevector-length bv)))
                            (let loop ((i 0) (count 0))
                              (unless (< i len) (error "utf8-index count too large" bv n count))
                              (if (< count n) (loop (utf8-next bv i) (+ count 1)) i))))

(define (unicode-control? c) (or (<= 0 c 31) (<= 127 c 159)))
(define (unicode-hspace? c) (case c ((9 11 12 32 160 8239 8287 12288) #t) (else (<= 8192 c 8202))))
(define (unicode-vspace? c) (case c ((10 13 133 8232 8233) #t) (else #f)))
(define (unicode-space? c) (or (<= 9 c 13)
                               (case c
                                 ((32 133 160 8232 8233 8239 8287 12288) #t)
                                 (else                                   (<= 8192 c 8202)))))
