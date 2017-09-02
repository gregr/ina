;; See: https://themonadreader.files.wordpress.com/2014/04/super-final.pdf

;; P ::= ((C1 arity1) ... (Cn arityn)) d1 ... dn
;; d ::= (define (f v1 ... vn) e)
;;     | (define (g p1 v1 ... vn) e1)
;;       ...
;;       (define (g pm v1 ... vn) em)
;; e ::= v
;;     | (C e1 ... en)
;;     | (f e1 ... en)
;; p ::= (C v1 ... vn)
