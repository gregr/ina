;; TODO: define utf8? and assert
(define (utf8->string bv) #;(assert (utf8? bv)) (bytevector->string bv))
(define (string->utf8 s)  (string->bytevector s))
