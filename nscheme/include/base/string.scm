(define (string-append . x*) (utf8->string (apply bytevector-append (map string->utf8 x*))))

;; TODO:
;string->number number->string
