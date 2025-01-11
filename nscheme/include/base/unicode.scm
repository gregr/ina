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
(define (utf8-encode-width c) (cond ((<= 0 c #x7f)   1)
                                    ((<= c #x7ff)    2)
                                    ((<= c #xffff)   3)
                                    ((<= c #x10ffff) 4)
                                    (else (error "not a unicode codepoint" c))))
(define (utf8-encode-2 c)
  (values (bitwise-ior #b11000000 (bitwise-asr c 6))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 c))))
(define (utf8-encode-3 c)
  (values (bitwise-ior #b11100000 (bitwise-asr c 12))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 (bitwise-asr c 6)))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 c))))
(define (utf8-encode-4 c)
  (values (bitwise-ior #b11110000 (bitwise-asr c 18))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 (bitwise-asr c 12)))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 (bitwise-asr c 6)))
          (bitwise-ior #b10000000 (bitwise-and #b00111111 c))))

(define (utf8-set!/width mbv i c width)
  (case width
    ((1) (mbytevector-set! mbv i c))
    ((2) (unless (< (+ i 1) (mbytevector-length mbv))
           (error "no space for 2-byte utf8 continuation" c mbv i (mbytevector-length mbv)))
         (let-values (((b0 b1) (utf8-encode-2 c)))
           (mbytevector-set! mbv i       b0)
           (mbytevector-set! mbv (+ i 1) b1)))
    ((3) (unless (< (+ i 2) (mbytevector-length mbv))
           (error "no space for 3-byte utf8 continuation" c mbv i (mbytevector-length mbv)))
         (let-values (((b0 b1 b2) (utf8-encode-3 c)))
           (mbytevector-set! mbv i       b0)
           (mbytevector-set! mbv (+ i 1) b1)
           (mbytevector-set! mbv (+ i 2) b2)))
    ((4) (unless (< (+ i 2) (mbytevector-length mbv))
           (error "no space for 4-byte utf8 continuation" c mbv i (mbytevector-length mbv)))
         (let-values (((b0 b1 b2 b3) (utf8-encode-4 c)))
           (mbytevector-set! mbv i       b0)
           (mbytevector-set! mbv (+ i 1) b1)
           (mbytevector-set! mbv (+ i 2) b2)
           (mbytevector-set! mbv (+ i 3) b3)))
    (else (error "not a utf8 width" c width))))
(define (utf8-set! mbv i c) (let ((width (utf8-encode-width c)))
                              (utf8-set!/width mbv i c width)
                              width))

(define (utf8-decode-width/k b0 kf k) (cond ((= (bitwise-and b0 #b10000000) #b00000000) (k 1))
                                            ((= (bitwise-and b0 #b11100000) #b11000000) (k 2))
                                            ((= (bitwise-and b0 #b11110000) #b11100000) (k 3))
                                            ((= (bitwise-and b0 #b11111000) #b11110000) (k 4))
                                            (else (kf "not a utf8 starting byte" b0))))
(define (utf8-decode-width b0) (utf8-decode-width/k b0 error (lambda (w) w)))
(splicing-local
  ((define (utf8-decode-continuation/k b kf k) (if (= (bitwise-and b #b11000000) #b10000000)
                                                   (k (bitwise-and b #x3f))
                                                   (kf "not a utf8 continuation byte" b))))
  (define (utf8-decode-2/k b0 b1 kf k)
    (let ((kf (lambda x* (apply kf "not a utf8 2-byte sequence" b0 b1 x*))))
      (if (< #b11000001 b0)
          (utf8-decode-continuation/k
            b1 kf (lambda (b1) (k (bitwise-ior (bitwise-asl (bitwise-and b0 #x1f) 6) b1))))
          (kf))))
  (define (utf8-decode-3/k b0 b1 b2 kf k)
    (let ((kf (lambda x* (apply kf "not a utf8 3-byte sequence" b0 b1 b2 x*))))
      (utf8-decode-continuation/k
        b1 kf
        (lambda (b1)
          (utf8-decode-continuation/k
            b2 kf
            (lambda (b2)
              (let ((c (bitwise-ior (bitwise-asl (bitwise-and b0 #x0f) 12) (bitwise-asl b1 6) b2)))
                (if (or (< #x7ff c #xd800) (< #xdfff c)) (k c) (kf)))))))))
  (define (utf8-decode-4/k b0 b1 b2 b3 kf k)
    (let ((kf (lambda x* (apply kf "not a utf8 4-byte sequence" b0 b1 b2 b3 x*))))
      (utf8-decode-continuation/k
        b1 kf
        (lambda (b1)
          (utf8-decode-continuation/k
            b2 kf
            (lambda (b2)
              (utf8-decode-continuation/k
                b3 kf
                (lambda (b3) (let ((c (bitwise-ior (bitwise-asl (bitwise-and b0 #x0f) 18)
                                                   (bitwise-asl b1 12)
                                                   (bitwise-asl b2 6)
                                                   b3)))
                               (if (< #xffff c #x110000) (k c) (kf)))))))))))
  (define (utf8-decode-2 b0 b1)       (utf8-decode-2/k b0 b1       error (lambda (c) c)))
  (define (utf8-decode-3 b0 b1 b2)    (utf8-decode-3/k b0 b1 b2    error (lambda (c) c)))
  (define (utf8-decode-4 b0 b1 b2 b3) (utf8-decode-4/k b0 b1 b2 b3 error (lambda (c) c))))

(define (utf8-ref/b0&width/k src i b0 width kf k)
  (let ((kf (lambda x* (apply kf "utf8 reference at" src i x*))))
    (if (= width 1)
        (k b0)
        (let-values (((len ref) (if (mbytevector? src)
                                    (values (mbytevector-length src) mbytevector-ref)
                                    (values (bytevector-length src) bytevector-ref))))
          (case width
            ((2) (if (< (+ i 1) len)
                     (utf8-decode-2/k b0 (ref src (+ i 1)) kf k)
                     (kf "missing 2-byte utf8 continuation" len)))
            ((3) (if (< (+ i 2) len)
                     (utf8-decode-3/k b0 (ref src (+ i 1)) (ref src (+ i 2)) kf k)
                     (kf "missing 3-byte utf8 continuation" len)))
            ((4) (if (< (+ i 3) len)
                     (utf8-decode-4/k b0 (ref src (+ i 1)) (ref src (+ i 2)) (ref src (+ i 3)) kf k)
                     (kf "missing 4-byte utf8 continuation" len)))
            (else (kf "not a utf8 width" width)))))))
(define (utf8-ref/b0&width bv i b0 width) (utf8-ref/b0&width/k bv i b0 width error (lambda (c) c)))
(define (utf8?/k bv kf kt)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0))
      (if (< i len)
          (let ((b0 (bytevector-ref bv i)))
            (utf8-decode-width/k
              b0 kf
              (lambda (width)
                (utf8-ref/b0&width/k bv i b0 width kf (lambda (_) (loop (+ i width)))))))
          (kt)))))
(define (utf8?! bv) (utf8?/k bv error         (lambda () (values))))
(define (utf8?  bv) (utf8?/k bv (lambda _ #f) (lambda () #t)))

(define (utf8-ref bv i) (let* ((b0 (bytevector-ref bv i)) (width (utf8-decode-width b0)))
                          (utf8-ref/b0&width bv i b0 width)))
(define (utf8-next bv i) (+ (utf8-decode-width (bytevector-ref bv i)) i))
(define (utf8-length bv) (let ((len (bytevector-length bv)))
                           (let loop ((i 0) (count 0))
                             (if (< i len) (loop (utf8-next bv i) (+ count 1)) count))))
(define (utf8-index bv start n)
  (let ((len (bytevector-length bv)))
    (let loop ((i start) (count 0))
      (cond ((<= n count) i)
            ((< i len)    (loop (utf8-next bv i) (+ count 1)))
            (else         (error "utf8-index out of bounds" bv start n count))))))

(define (unicode?         c) (and (<= 0 c #x10ffff) (not (<= #xd800 c #xdfff))))
(define (unicode-control? c) (or (<= 0 c 31) (<= 127 c 159)))
(define (unicode-hspace?  c) (case c ((9 32 160 5760 8239 8287 12288) #t) (else (<= 8192 c 8202))))
(define (unicode-vspace?  c) (or (<= 10 c 13) (case c ((133 8232 8233) #t) (else #f))))
(define (unicode-space?   c) (or (unicode-hspace? c) (unicode-vspace? c)))
