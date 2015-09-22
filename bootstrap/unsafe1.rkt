#lang racket/base
(provide
  )

(require
  "term.rkt"
  "unsafe0.rkt"
  racket/function
  )

(define pcons '(lambda (hd tl) (pair hd tl)))

(define tag:integer '(pair 1 (pair 0 (pair 0 ()))))

(define bits-nil '(pair 0 ()))
(define bits '(lambda (b0 bs) (pair 1 (pair b0 bs))))

(define (nat->bits n (invert? #f))
  (if (= 0 n) bits-nil
    `(,bits ,(if ((if invert? not identity) (= 0 (modulo n 2))) 0 1)
            ,(nat->bits (quotient n 2) invert?))))

(define (int->integer i)
  `(,pcons ,tag:integer (,pcons ,(if (< i 0) 1 0)
                                ,(nat->bits (if (< i 0) (- (+ i 1)) i)
                                            (< i 0)))))

(define std0-module (unsafe0-module
  `((identity (lambda (x) x))
    (const    (lambda (k _) k))
    (compose  (lambda (f g x) (f (g x))))
    (fix (lambda (f) ((lambda (d) (d d))
                      (lambda (x) (f (lambda (a) (x x a)))))))

    (pcons    ,pcons)
    (phead    (lambda (pr)    (unpair 0 pr)))
    (ptail    (lambda (pr)    (unpair 1 pr)))
    (psecond  (lambda (pr)    (phead (ptail pr))))
    (pthird   (lambda (pr)    (phead (ptail (ptail pr)))))
    (puncurry (lambda (f pr)  (f (phead pr) (ptail pr))))

    (not?0  (lambda (b0)    (if0 b0 1 0)))
    (bit=?0 (lambda (b0 b1) (if0 b0 (if0 b1 0 1) (if0 b1 1 0))))

    (tag:symbol  (pair 0 (pair 0 (pair 0 ()))))
    (tag:boolean (pair 0 (pair 0 (pair 1 ()))))
    (tag:nil     (pair 0 (pair 1 (pair 0 ()))))
    (tag:cons    (pair 0 (pair 1 (pair 1 ()))))
    (tag:integer ,tag:integer)
    (tag=?0 (lambda (t0 t1)
              (if0 (bit=?0 (phead t0) (phead t1))
                   (if0 (bit=?0 (psecond t0) (psecond t1))
                        (if0 (bit=?0 (pthird t0) (pthird t1))
                             0 1) 1) 1)))

    (tagged          pcons)
    (tagged->tag     phead)
    (tagged->payload ptail)

    (tagged-map (lambda (tag f datum)
                  (if0 (tag=?0 tag (tagged->tag datum))
                       (f (tagged->payload datum))
                       (() ()))))
    (tagged-binop (lambda (tag op)
                    (tagged-map tag (compose (tagged-map tag) op))))

    (bit->boolean   (tagged tag:boolean))
    (tag=?    (lambda (t0 t1) (bit->boolean (tag=?0 t0 t1))))
    (has-tag? (lambda (tag) (compose (tag=? tag) tagged->tag)))

    (true           (bit->boolean 0))
    (false          (bit->boolean 1))
    (boolean->bit   (tagged-map tag:boolean identity))
    (not?           (compose not?0 boolean->bit) )
    (boolean-unpair (lambda (b pr) (unpair (boolean->bit b) pr)))
    (boolean?       (has-tag? tag:boolean))

    (bits-nil   ,bits-nil)
    (bits       ,bits)
    (bits-nil?0 (lambda (bs) (phead bs)))
    (bits-head  psecond)
    (bits-tail  (compose ptail ptail))
    (bits-cocase
      (lambda (c__ c_x cx_ c00 c10 c01 c11 bs0 bs1)
        (if0 (bits-nil?0 bs0)
             (if0 (bits-nil?0 bs1) (c__ ()) (c_x bs1))
             (if0 (bits-nil?0 bs1) (cx_ bs0)
                  (if0 (bits-head bs0)
                       (if0 (bits-head bs1)
                            (c00 (bits-tail bs0) (bits-tail bs1))
                            (c01 (bits-tail bs0) (bits-tail bs1)))
                       (if0 (bits-head bs1)
                            (c10 (bits-tail bs0) (bits-tail bs1))
                            (c11 (bits-tail bs0) (bits-tail bs1))))))))
    (bits=? (fix (lambda (bits=? bs0 bs1)
                   (bits-cocase (const true) (const false) (const false)
                                bits=? (const (const false))
                                (const (const false)) bits=?
                                bs0 bs1))))

    (symbol   (tagged tag:symbol))
    (symbol?  (has-tag? tag:symbol))
    (symbol=? (tagged-binop tag:symbol bits=?))

    (integer   (tagged tag:integer))
    (integer?  (has-tag? tag:integer))
    (integer-compare->case
      (lambda (=case <case >case)
        (tagged-binop
          tag:integer
          (lambda (ir0 ir1)
            (let ((<case (if0 (phead ir0) <case >case))
                  (>case (if0 (phead ir0) >case <case))
                  (cmp (fix (lambda (cmp =case <case >case bs0 bs1)
                              (bits-cocase
                                (const =case) (const <case) (const >case)
                                (cmp =case <case >case) (cmp >case <case >case)
                                (cmp <case <case >case) (cmp =case <case >case)
                                bs0 bs1)))))
              (if0 (bit=?0 (phead ir0) (phead ir1))
                   (cmp =case <case >case (ptail ir0) (ptail ir1))
                   >case))))))
    (integer=?  (integer-compare->case true false false))
    (integer<?  (integer-compare->case false true false))
    (integer<=? (integer-compare->case true true false))
    (integer>?  (integer-compare->case false false true))
    (integer>=? (integer-compare->case true false true))
    (integer+
      (tagged-binop
        tag:integer
        (lambda (ir0 ir1)
          (let*
            ((ext0 (phead ir0))
             (ext1 (phead ir1))
             (add3 (lambda (b0 b1 carry)
                     (if0 b0 (if0 b1 (pair carry 0)
                                  (if0 carry (pair 1 0) (pair 0 1)))
                          (if0 b1 (if0 carry (pair 1 0) (pair 0 1))
                               (pair carry 1)))))
             (trimmed
               (lambda (current rest)
                 (if0 (phead rest)
                      (pcons 0 (pcons (phead (ptail rest))
                                      (bits current (ptail (ptail rest)))))
                      (if0 (bit=?0 current (ptail rest)) rest
                           (pcons
                             0 (pcons (ptail rest)
                                      (bits current bits-nil)))))))
             (return (lambda (result)
                       (if0 (phead result) (ptail result)
                            (pcons (ptail result) bits-nil))))
             (add
               (fix
                 (lambda (add carry)
                   (let ((trimmed/f
                           (lambda (f b0 b1 carry bs0 bs1)
                             (let* ((rc (add3 b0 b1 carry))
                                    (result (phead rc))
                                    (carry  (ptail rc)))
                               (trimmed result (f carry bs0 bs1))))))
                     (bits-cocase
                       (lambda (_)
                         (trimmed/f (lambda (carry _ _)
                                      (pcons 1 (phead (add3 ext0 ext1 carry))))
                                    ext0 ext1 carry () ()))
                       (lambda (bs1) (add carry (bits ext0 bits-nil) bs1))
                       (lambda (bs0) (add carry bs0 (bits ext1 bits-nil)))
                       (trimmed/f add 0 0 carry)
                       (trimmed/f add 1 0 carry)
                       (trimmed/f add 0 1 carry)
                       (trimmed/f add 1 1 carry)))))))
            (tagged tag:integer (return (add 0 (ptail ir0) (ptail ir1))))))))

    (nil     (tagged tag:nil ()))
    (cons    (lambda (hd tl) (tagged tag:cons (pcons hd tl))))
    (head    (tagged-map tag:cons phead))
    (tail    (tagged-map tag:cons ptail))
    (uncurry (lambda (f) ((tagged-map tag:cons (puncurry f)))))
    (nil?    (has-tag? tag:nil))
    (cons?   (has-tag? tag:cons))
    )))

(define std0 (compose t-value (curry hash-ref std0-module)))

(module+ test
  (require
    "denotation.rkt"
    gregr-misc/sugar
    rackunit
    )

  (define (std0-apply stx . std0-idents)
    (forf proc = (unsafe0-parse stx)
          ident <- std0-idents
          (t-apply proc (std0 ident))))

  (check-equal?
    ((denote (t-apply (std0 'identity) (t-value (v-unit))))
     env-empty)
    '())
  (check-equal?
    ((denote
       (std0-apply '(lambda (psecond)
                      (psecond (pair (pair 1 0) (pair (pair 0 1) ()))))
                   'psecond))
     env-empty)
    '(0 . 1))
  (check-equal?
    ((denote
       (std0-apply '(lambda (cons cons?) (cons? (cons () ()))) 'cons 'cons?))
     env-empty)
    ((denote (std0 'true)) env-empty))
  (check-equal?
    ((denote
       (std0-apply '(lambda (cons nil tail nil?) (nil? (tail (cons () nil))))
                   'cons 'nil 'tail 'nil?))
     env-empty)
    ((denote (std0 'true)) env-empty))

  (check-equal?
    ((denote (std0-apply
               `(lambda (pcons bits=?)
                  (pcons (bits=? ,(nat->bits 3) ,(nat->bits 6))
                         (bits=? ,(nat->bits 6) ,(nat->bits 6))))
               'pcons 'bits=?))
     env-empty)
    ((denote (std0-apply `(lambda (true false) (pair false true))
                         'true 'false)) env-empty))

  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -1) ,(int->integer 1)))
                   'integer<?))
     env-empty)
    ((denote (std0 'true)) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 6)))
                   'integer=?))
     env-empty)
    ((denote (std0 'true)) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 7)))
                   'integer=?))
     env-empty)
    ((denote (std0 'false)) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer>?))
     env-empty)
    ((denote (std0 'true)) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer<=?))
     env-empty)
    ((denote (std0 'false)) env-empty))

  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 1) ,(int->integer 2)))
                   'integer+))
     env-empty)
    ((denote (unsafe0-parse (int->integer 3))) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer+))
     env-empty)
    ((denote (unsafe0-parse (int->integer 9))) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -7) ,(int->integer 3)))
                   'integer+))
     env-empty)
    ((denote (unsafe0-parse (int->integer -4))) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -2) ,(int->integer -5)))
                   'integer+))
     env-empty)
    ((denote (unsafe0-parse (int->integer -7))) env-empty))
  (check-equal?
    ((denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 5) ,(int->integer -5)))
                   'integer+))
     env-empty)
    ((denote (unsafe0-parse (int->integer 0))) env-empty))
  )
