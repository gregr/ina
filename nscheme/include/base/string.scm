(splicing-local
  ((define byte:+ 43)
   (define byte:- 45)
   (define byte:0 48) (define byte:A 65) (define byte:a 97)
   (define byte:/ 47) (define byte:. 46) (define byte:~ 126)
   (define byte:e 101) (define byte:E 69)
   (define byte:p 112) (define byte:P 80)
   (define byte:# 35) (define byte:b 98) (define byte:d 100) (define byte:o 111) (define byte:x 120)
   (define byte:B 66) (define byte:D 68) (define byte:O 79) (define byte:X 88)
   (define (digit16 d)
     (if (<= byte:A d)
         (if (<= byte:a d) (+ (- d byte:a) 10) (+ (- d byte:A) 10))
         (- d byte:0)))
   (define (radix?! r) (unless (and (integer? r) (<= 2 r 16)) (error "unsupported radix" r)))
   (define (set-nat-radix-digits! mbv start count n radix)
     (let loop ((i (+ start count -1)) (n n))
       (when (<= start i)
         (let-values (((q r) (integer-floor-divmod n radix)))
           (mbytevector-set! mbv i (if (< r 10) (+ r byte:0) (+ (- r 10) byte:a)))
           (loop (- i 1) q)))))
   (define (rhs-digit-counts n radix)
     (define non-repeating?
       (let ((check1 (lambda (a) (lambda (n) (= (integer-floor-mod (denominator n) a) 0))))
             (check2 (lambda (a b) (lambda (n) (let ((d (denominator n)))
                                                 (or (= (integer-floor-mod d a) 0)
                                                     (= (integer-floor-mod d b) 0)))))))
         (cond ((= (integer-floor-mod radix 2) 0)
                (cond ((= (integer-floor-mod radix 3) 0) (check2 2 3))
                      ((= (integer-floor-mod radix 5) 0) (check2 2 5))
                      ((= (integer-floor-mod radix 7) 0) (check2 2 7))
                      (else                              (check1 2))))
               ((= (integer-floor-mod radix 3) 0)
                (if (= (integer-floor-mod radix 5) 0) (check2 3 5) (check1 3)))
               (else (check1 radix)))))
     (let loop-non-repeating ((n n) (non-repeating-count 0))
       (cond ((= n 0) (values non-repeating-count 0))
             ((non-repeating? n) (let* ((m (* n radix)) (n (- m (floor m))))
                                   (loop-non-repeating n (+ non-repeating-count 1))))
             (else (let ((start n))
                     (let loop-repeating ((n n) (repeating-count 1))
                       (let* ((m (* n radix)) (n (- m (floor m))))
                         (if (= n start)
                             (values non-repeating-count repeating-count)
                             (loop-repeating n (+ repeating-count 1))))))))))
   (define (style-ref style key)
     (and (pair? style) (let ((kv (assq key style))) (and kv (cdr kv)))))
   (define (integer->utf8 n radix)
     (radix?! radix)
     (cond ((< n 0) (let* ((len (+ (floor-log (- n) radix) 2))
                           (mbv (make-mbytevector len 0)))
                      (mbytevector-set! mbv 0 byte:-)
                      (set-nat-radix-digits! mbv 1 (- len 1) (- n) radix)
                      (mbytevector->bytevector mbv)))
           ((< 0 n) (let* ((len (+ (floor-log n radix) 1))
                           (mbv (make-mbytevector len 0)))
                      (set-nat-radix-digits! mbv 0 len n radix)
                      (mbytevector->bytevector mbv)))
           (else #"0")))
   (define (fraction->utf8 n radix)
     (radix?! radix)
     (bytevector-append (integer->utf8 (numerator n) radix) #"/"
                        (integer->utf8 (denominator n) radix)))
   (define (decimal->utf8 n radix radix.exponent use-exponent-below use-exponent-above)
     (when radix.exponent (radix?! radix.exponent))
     (let ((radix.exponent (or radix.exponent radix)))
       (radix?! radix)
       (when (and use-exponent-below (< 1 use-exponent-below))
         (error "use-exponent-below is larger than 1" use-exponent-below))
       (when (and use-exponent-above (< use-exponent-above radix))
         (error "use-exponent-above is smaller than radix" use-exponent-above radix))
       (let ((mag (abs n)))
         (define (go power)
           (let* ((power-mag         (abs power))
                  (power-digit-count (if (= power 0) 0 (+ (floor-log power-mag radix.exponent) 1)))
                  (mag               (/ mag (expt radix power)))
                  (whole             (floor mag))
                  (whole-digit-count (if (= whole 0) 1 (+ (floor-log whole radix) 1)))
                  (part              (- mag whole)))
             (let-values (((non-repeating-count repeating-count) (rhs-digit-counts part radix)))
               (let* ((part-len      (+ non-repeating-count repeating-count))
                      (part-len      (if (< 0 part-len) (+ part-len 1) 0))
                      (part-len      (if (< 0 repeating-count) (+ part-len 1) part-len))
                      (part          (* (expt radix non-repeating-count) part))
                      (non-repeating (floor part))
                      (part          (- part non-repeating))
                      (repeating     (floor (* (expt radix repeating-count) part)))
                      (len           (+ whole-digit-count part-len power-digit-count
                                        (if (< n 0) 1 0)
                                        (cond ((< power 0) 2) ((= power 0) 0) (else 1))))
                      (mbv           (make-mbytevector len 0)))
                 (let* ((i (if (< n 0)
                               (begin (mbytevector-set! mbv 0 byte:-) 1)
                               0))
                        (i (begin (set-nat-radix-digits! mbv i whole-digit-count whole radix)
                                  (+ i whole-digit-count)))
                        (i (if (< 0 part-len)
                               (begin
                                 (mbytevector-set! mbv i byte:.)
                                 (let* ((i (+ i 1))
                                        (i (begin (set-nat-radix-digits! mbv i non-repeating-count
                                                                         non-repeating
                                                                         radix)
                                                  (+ i non-repeating-count))))
                                   (if (< 0 repeating-count)
                                       (begin (mbytevector-set! mbv i byte:~)
                                              (let ((i (+ i 1)))
                                                (set-nat-radix-digits! mbv i repeating-count
                                                                       repeating
                                                                       radix)
                                                (+ i repeating-count)))
                                       i)))
                               i))
                        (i (if (< 0 power-digit-count)
                               (begin (mbytevector-set! mbv i (if (<= radix 10) byte:e byte:p))
                                      (+ i 1))
                               i))
                        (i (if (< power 0)
                               (begin (mbytevector-set! mbv i byte:-)
                                      (+ i 1))
                               i)))
                   (set-nat-radix-digits! mbv i power-digit-count power radix.exponent)
                   (mbytevector->bytevector mbv))))))
         (cond
           ((and use-exponent-below (< mag use-exponent-below)) (go (floor-log mag radix)))
           ((and use-exponent-above (< use-exponent-above mag)) (go (floor-log mag radix)))
           (else                                                (go 0)))))))
  (define number->utf8
    (let ((go (lambda (n radix style)
                (cond ((integer? n)
                       (let ((uea (style-ref style 'use-exponent-above)))
                         (if (and uea (< uea (abs n)))
                             (decimal->utf8 n radix (style-ref style 'exponent-radix) #f uea)
                             (integer->utf8 n radix))))
                      ((or (not style) (eq? style 'fraction)) (fraction->utf8 n radix))
                      (else (decimal->utf8 n radix (style-ref style 'exponent-radix)
                                           (style-ref style 'use-exponent-below)
                                           (style-ref style 'use-exponent-above)))))))
      (case-lambda
        ((n)             (go n 10    #f))
        ((n radix)       (go n radix #f))
        ((n radix style) (go n radix style)))))
  (define utf8->number
    (let ((go (lambda (bv radix)
                (radix?! radix)
                (let ((len (bytevector-length bv)))
                  (define (Radix radix i k)
                    (and (< i len)
                         (let ((b (bytevector-ref bv i)))
                           (if (= b byte:#)
                               (let ((i (+ i 1)))
                                 (and (< i len)
                                      (let ((b (bytevector-ref bv i)))
                                        (let ((i (+ i 1)))
                                          (and (< i len)
                                               (let ((c (bytevector-ref bv i)))
                                                 (cond
                                                   ((or (= b byte:b) (= b byte:B)) (k 2 i c))
                                                   ((or (= b byte:o) (= b byte:O)) (k 8 i c))
                                                   ((or (= b byte:d) (= b byte:D)) (k 10 i c))
                                                   ((or (= b byte:x) (= b byte:X)) (k 16 i c))
                                                   (else #f))))))))
                               (k radix i b)))))
                  (define (Sign i b k)
                    (define (next sign)
                      (let ((i (+ i 1)))
                        (and (< i len) (k sign i (bytevector-ref bv i)))))
                    (cond ((= b byte:-) (next -1))
                          ((= b byte:+) (next  1))
                          (else         (k 1 i b))))
                  (define (RadixSign radix i k)
                    (Radix radix i (lambda (radix i b) (Sign i b (lambda (sign i b)
                                                                   (k radix sign i b))))))
                  (define (Digit* radix i b k.more k.end)
                    (let loop ((n 0) (digit-count 0) (i i) (b b))
                      (let ((x (digit16 b)))
                        (if (< -1 x radix)
                            (let ((n (+ (* n radix) x)) (digit-count (+ digit-count 1)) (i (+ i 1)))
                              (if (< i len)
                                  (loop n digit-count i (bytevector-ref bv i))
                                  (k.end n digit-count)))
                            (k.more n digit-count i b)))))
                  (define (Digit+ radix i k.more k.end)
                    (if (< i len)
                        (Digit* radix i (bytevector-ref bv i) k.more k.end)
                        (k.end 0 0)))
                  (define (Exp n radix i b)
                    (and (or (= b byte:e) (= b byte:E) (= b byte:p) (= b byte:P))
                         (RadixSign radix (+ i 1)
                                    (lambda (radix sign i b)
                                      (Digit* radix i b
                                              (lambda (x digit-count i b) #f)
                                              (lambda (x digit-count)
                                                (and (< 0 digit-count)
                                                     (* n (expt radix (* sign x))))))))))
                  (RadixSign
                    radix 0
                    (lambda (radix sign i b)
                      (define (decimal lhs lhs-digit-count rhs rhs-digit-count rep rep-digit-count)
                        (and (or (< 0 lhs-digit-count) (< 0 rhs-digit-count))
                             (let ((rhs (if (< 0 rep-digit-count)
                                            (+ rhs (/ rep (- (expt radix rep-digit-count) 1)))
                                            rhs)))
                               (* sign (+ lhs (/ rhs (expt radix rhs-digit-count)))))))
                      (Digit*
                        radix i b
                        (lambda (n digit-count i b)
                          (if (= b byte:.)
                              (Digit+
                                radix (+ i 1)
                                (lambda (rhs rhs-digit-count i b)
                                  (if (= b byte:~)
                                      (Digit+
                                        radix (+ i 1)
                                        (lambda (repeating repeating-digit-count i b)
                                          (and (< 0 repeating-digit-count)
                                               (let ((n (decimal n digit-count rhs rhs-digit-count
                                                                 repeating
                                                                 repeating-digit-count)))
                                                 (and n (Exp n radix i b)))))
                                        (lambda (repeating repeating-digit-count)
                                          (and (< 0 repeating-digit-count)
                                               (decimal n digit-count rhs rhs-digit-count repeating
                                                        repeating-digit-count))))
                                      (let ((n (decimal n digit-count rhs rhs-digit-count 0 0)))
                                        (and n (Exp n radix i b)))))
                                (lambda (rhs rhs-digit-count)
                                  (decimal n digit-count rhs rhs-digit-count 0 0)))
                              (and (< 0 digit-count)
                                   (if (= b byte:/)
                                       (Digit+ radix (+ i 1)
                                               (lambda (d digit-count i b) #f)
                                               (lambda (d digit-count)
                                                 (and (< 0 digit-count)
                                                      (* sign (/ n d)))))
                                       (Exp (* sign n) radix i b)))))
                        (lambda (n digit-count)
                          (and (< 0 digit-count) (* sign n))))))))))
      (case-lambda
        ((bv)       (go bv 10))
        ((bv radix) (go bv radix))))))

(define (number->string . x*) (utf8->string (apply number->utf8 x*)))
(define (string->number s . option*) (apply utf8->number (string->utf8 s) option*))

(define (string-append* x*) (utf8->string (bytevector-append* (map string->utf8 x*))))
(define (string-append . x*) (string-append* x*))
(define (string-join* separator x*)
  (utf8->string (bytevector-join* (string->utf8 separator) (map string->utf8 x*))))
(define (string-join separator . x*) (string-join* separator x*))

(define (make-local-gensym)
  (mlet ((count -1))
    (lambda (name)
      (set! count (+ count 1))
      (let ((name (cond ((bytevector? name) name)
                        ((string?     name) (string->utf8 name))
                        ((symbol?     name) (string->utf8 (symbol->string name)))
                        (else               (error "not a symbol, string, or bytevector" name)))))
        (string->symbol (utf8->string (bytevector-append name #"." (number->utf8 count))))))))
