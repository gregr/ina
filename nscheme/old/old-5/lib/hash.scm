((provide hash))

;; TODO: define general and maybe rolling hash functions.
;; For simplicity start with FNV-1a.
;; Maybe define a rolling variant using + instead of xor.

;; hash-null
;; hash-boolean
;; hash-char
;; hash-number
;; hash-string
;; hash-pair
;; hash-vector

(define (hash d)
  ((cond ((null? d)    hash-null)
         ((boolean? d) hash-boolean)
         ((char? d)    hash-char)
         ((number? d)  hash-number)
         ((string? d)  hash-string)
         ((pair? d)    hash-pair)
         ((vector? d)  hash-vector)
         (#t (error '"cannot hash:" d))) d))
