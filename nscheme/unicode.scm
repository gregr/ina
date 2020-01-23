(define unicode-min 0)
(define unicode-max #x7FFFFFFF)
(define (unicode? i) (and (integer? i) (<= 0 i unicode-max)))

;; Conventionally invalid ranges: U+D800 through U+DFFF and above U+10FFFF
;; This encoding could handle up to 7 units (42 bits), i.e., U+3FFFFFFFFFF
;; These octets happen to never appear:
;; C0: #b11000000
;; C1: #b11000001
;; F5: #b11110101
;; FF: #b11111111
(define unicode-ranges
  ;;   rmin         rmax         tag-mask   tag        shift
  `#(#(,unicode-min #x0000007F   #b10000000 #b00000000 0)
     #(#x0000080    #x000007FF   #b11100000 #b11000000 6)
     #(#x0000800    #x0000FFFF   #b11110000 #b11100000 12)
     #(#x0010000    #x001FFFFF   #b11111000 #b11110000 18)
     #(#x0200000    #x03FFFFFF   #b11111100 #b11111000 24)
     #(#x4000000    ,unicode-max #b11111110 #b11111100 30)))

(define (unicode->utf8 i)
  (and (integer? i) (<= 0 i)
       (let loop ((ri 0))
         (and (< ri (vector-length unicode-ranges))
              (let ((r (vector-ref unicode-ranges ri)))
                (match-define (vector rmin rmax tag-mask tag shift) r)
                (if (<= i rmax)
                  (cons (bitwise-ior tag (>> i shift))
                        (let loop ((s (- shift 6)))
                          (if (<= 0 s)
                            (cons (\| #x80 (& #x3F (>> i s))) (loop (- s 6)))
                            '())))
                  (loop (+ ri 1))))))))

(define (utf8->unicode i0)
  (let loop ((ri 0) (i i0))
    (and (< ri (vector-length unicode-ranges))
         (let ((r (vector-ref unicode-ranges ri)))
           (match-define (vector rmin rmax tag-mask tag shift) r)
           (if (= (& i0 tag-mask) tag)
             (let ((i (& i rmax))) (and (< rmin i) i))
             (lambda (inext) (let ((inext (^ inext #x80)))
                               (and (= 0 (& inext #xC0))
                                    (loop (+ ri 1) (\| (<< i 6) inext))))))))))

(define (string->unicodes s)
  (define result
    (foldl (lambda (b acc) (let ((rps (car acc)) (next ((cdr acc) b)))
                             (if (procedure? next) (cons rps next)
                               (cons (cons next rps) utf8->unicode))))
           (cons '() utf8->unicode) (string->list s)))
  (reverse (append (if (eq? (cdr result) utf8->unicode) '() '(#f))
                   (car result))))
