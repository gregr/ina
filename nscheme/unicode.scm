(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)
(define \| bitwise-ior)
(define &  bitwise-and)
(define ^  bitwise-xor)

;; TODO: rename unicode to codepoint in many places
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

;; TODO: codepoint->utf8
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

;; TODO: utf8->codepoint
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

;; TODO: work with (byte)vectors of utf8 codeunits rather than strings.
;; Can then define a string operator from this.
(define (string->unicodes s)
  (define result
    (foldl (lambda (b acc) (let ((rps (car acc)) (next ((cdr acc) b)))
                             (if (procedure? next) (cons rps next)
                               (cons (cons next rps) utf8->unicode))))
           (cons '() utf8->unicode) (bytevector->u8* (string->utf8 s))))
  (reverse (append (if (eq? (cdr result) utf8->unicode) '() '(#f))
                   (car result))))

(define (intervals-subtract rs1 rs2)
  (cond ((or (null? rs1) (null? rs2)) rs1)
        ((< (cdar rs1) (caar rs2))
         (cons (car rs1) (intervals-subtract (cdr rs1) rs2)))
        ((< (cdar rs2) (caar rs1)) (intervals-subtract rs1 (cdr rs2)))
        (else (append (if (<= (caar rs2) (caar rs1)) '()
                        (list (cons (caar rs1) (- (caar rs2) 1))))
                      (intervals-subtract
                        (append (if (<= (cdar rs1) (cdar rs2)) '()
                                  (list (cons (+ (cdar rs2) 1) (cdar rs1))))
                                (cdr rs1))
                        (append (if (<= (cdar rs2) (cdar rs1)) '()
                                  (list (cons (+ (cdar rs1) 1) (cdar rs2))))
                                (cdr rs2)))))))
(define (intervals-merge rs1 rs2)
  (cond ((null? rs1) rs2)
        ((null? rs2) rs1)
        ((= (+ (cdar rs1) 1) (caar rs2))
         (intervals-merge (cdr rs1) (cons (cons (caar rs1) (cdar rs2))
                                          (cdr rs2))))
        ((= (+ (cdar rs2) 1) (caar rs1))
         (intervals-merge (cons (cons (caar rs2) (cdar rs1)) (cdr rs1))
                          (cdr rs2)))
        ((< (cdar rs1) (caar rs2))
         (cons (car rs1) (intervals-merge (cdr rs1) rs2)))
        ((< (cdar rs2) (caar rs1))
         (cons (car rs2) (intervals-merge rs1 (cdr rs2))))
        (else (cons (cons (min (caar rs1) (caar rs2))
                          (max (cdar rs1) (cdar rs2)))
                    (intervals-merge (cdr rs1) (cdr rs2))))))
(define (intset->intervals iset)
  (map (lambda (i) (if (number? i) (cons i i) i)) (vector->list iset)))
(define (intervals->intset ivals)
  (list->vector (map (lambda (i) (if (= (car i) (cdr i)) (car i) i)) ivals)))
(define (intset-merge is1 is2)
  (intervals->intset (intervals-merge (intset->intervals is1)
                                      (intset->intervals is2))))
(define (intset-member? s i)
  (let loop ((start 0) (end (vector-length s)))
    (and (< start end)
         (let* ((n (+ start (>> (- end start) 1)))
                (r (vector-ref s n)))
           (cond ((integer? r) (if (< i r) (loop start n)
                                 (or (= i r) (loop (+ n 1) end))))
                 (else (if (< i (car r)) (loop start n)
                         (or (<= i (cdr r)) (loop (+ n 1) end)))))))))

;; TODO: codepoints.hspace etc.
(define unicode-hspace '#(9 (11 . 12) 32 160 (8192 . 8202) 8239 8287 12288))
(define unicode-vspace '#(10 13 133 8232 8233))
(define unicode-space (intset-merge unicode-hspace unicode-vspace))
(define unicode-control '#((0 . 31) (127 . 159)))

;; TODO: codepoint-hspace? etc.
(define (unicode-hspace?  i) (intset-member? unicode-hspace  i))
(define (unicode-vspace?  i) (intset-member? unicode-vspace  i))
(define (unicode-space?   i) (intset-member? unicode-space   i))
(define (unicode-control? i) (intset-member? unicode-control i))

;; TODO: support entire unicode code points
(define-syntax (char stx)
  (syntax-case stx ()
    ((_ ss) (let ((s (syntax->datum #'ss)))
              (define (string-length s) (bytevector-length (string->utf8 s)))
              ;; TODO: do not use string-length
              (and (string? s) (= (string-length s) 1)))
            (let ()
              (define (string-ref s i) (bytevector-u8-ref (string->utf8 s) i))
              ;; TODO: do not use string-ref
              (datum->syntax #'_ (string-ref (syntax->datum #'ss) 0))))))
