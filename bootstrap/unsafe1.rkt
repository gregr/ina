#lang racket/base
(provide
  unsafe1-parse
  unsafe1-module
  )

(require
  "operation.rkt"
  "parsing.rkt"
  "substitution.rkt"
  "term.rkt"
  "unsafe0.rkt"
  gregr-misc/dict
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/record
  gregr-misc/sugar
  racket/function
  racket/match
  )

(module+ test
  (require
    "denotation.rkt"
    rackunit
    ))

(define pcons '(lambda (hd tl) (pair hd tl)))

(define tag:symbol '(pair 0 (pair 0 (pair 0 ()))))
(define tag:integer '(pair 1 (pair 0 (pair 0 ()))))

(define bits-nil '(pair 0 ()))
(define bits '(lambda (b0 bs) (pair 1 (pair b0 bs))))

(define (nat->bits n (invert? #f))
  (if (= 0 n) bits-nil
    `(,bits ,(if ((if invert? not identity) (= 0 (modulo n 2))) 0 1)
            ,(nat->bits (quotient n 2) invert?))))

(define (nat->symbol n)
  (step-complete (unsafe0-parse `(,pcons ,tag:symbol ,(nat->bits n)))))

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

    (not?0  (lambda (b0)    (if0 b0 1 0)))
    (bit=?0 (lambda (b0 b1) (if0 b0 (if0 b1 0 1) (if0 b1 1 0))))

    (tag:symbol  ,tag:symbol)
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
    (bits-invert
      (fix (lambda (bits-invert bs)
        (if0 (bits-nil?0 bs) bs
             (bits (not?0 (bits-head bs)) (bits-invert (bits-tail bs)))))))
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
    (integer-invert-naive
      (tagged-map
        tag:integer
        (lambda (ir) (tagged tag:integer (pcons (not?0 (phead ir))
                                                (bits-invert (ptail ir)))))))
    (zero         (tagged tag:integer (pcons 0 bits-nil)))
    (negative-one (tagged tag:integer (pcons 1 bits-nil)))
    (positive-one (tagged tag:integer (pcons 0 (bits 1 bits-nil))))
    (integer-invert
      (lambda (int)
        (if0 (boolean->bit (integer<=? zero int))
             (integer-invert-naive (integer+ negative-one int))
             (integer+ positive-one (integer-invert-naive int)))))

    (nil     (tagged tag:nil ()))
    (cons    (lambda (hd tl) (tagged tag:cons (pcons hd tl))))
    (head    (tagged-map tag:cons phead))
    (tail    (tagged-map tag:cons ptail))
    (nil?    (has-tag? tag:nil))
    (cons?   (has-tag? tag:cons))
    )))

(define std0 (compose t-value (curry hash-ref std0-module)))

(module+ test
  (define (std0-apply stx . std0-idents)
    (build-apply (unsafe0-parse stx) (map std0 std0-idents)))

  (check-equal?
    (denote (t-apply (std0 'identity) (t-value (v-unit))))
    '())
  (check-equal?
    (denote
       (std0-apply '(lambda (psecond)
                      (psecond (pair (pair 1 0) (pair (pair 0 1) ()))))
                   'psecond))
    '(0 . 1))
  (check-equal?
    (denote
       (std0-apply '(lambda (cons cons?) (cons? (cons () ()))) 'cons 'cons?))
    (denote (std0 'true)))
  (check-equal?
    (denote
       (std0-apply '(lambda (cons nil tail nil?) (nil? (tail (cons () nil))))
                   'cons 'nil 'tail 'nil?))
    (denote (std0 'true)))

  (check-equal?
    (denote (std0-apply
               `(lambda (pcons bits=?)
                  (pcons (bits=? ,(nat->bits 3) ,(nat->bits 6))
                         (bits=? ,(nat->bits 6) ,(nat->bits 6))))
               'pcons 'bits=?))
    (denote (std0-apply `(lambda (true false) (pair false true))
                         'true 'false)))

  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -1) ,(int->integer 1)))
                   'integer<?))
    (denote (std0 'true)))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 6)))
                   'integer=?))
    (denote (std0 'true)))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 7)))
                   'integer=?))
    (denote (std0 'false)))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer>?))
    (denote (std0 'true)))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer<=?))
    (denote (std0 'false)))

  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 1) ,(int->integer 2)))
                   'integer+))
    (denote (unsafe0-parse (int->integer 3))))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 6) ,(int->integer 3)))
                   'integer+))
    (denote (unsafe0-parse (int->integer 9))))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -7) ,(int->integer 3)))
                   'integer+))
    (denote (unsafe0-parse (int->integer -4))))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer -2) ,(int->integer -5)))
                   'integer+))
    (denote (unsafe0-parse (int->integer -7))))
  (check-equal?
    (denote
       (std0-apply `(lambda (iop) (iop ,(int->integer 5) ,(int->integer -5)))
                   'integer+))
    (denote (unsafe0-parse (int->integer 0))))

  (check-equal?
    (denote (std0-apply `(lambda (iop) (iop ,(int->integer 5)))
                         'integer-invert))
    (denote (unsafe0-parse (int->integer -5))))
  (check-equal?
    (denote (std0-apply `(lambda (iop) (iop (iop ,(int->integer -5))))
                         'integer-invert))
    (denote (unsafe0-parse (int->integer -5))))
  )

(define (parse-extra senv stx)
  (match stx
    (#t           (std0 'true))
    (#f           (std0 'false))
    ((? integer?) (step-complete (unsafe0-parse (int->integer stx))))
    (_            (error (format "invalid syntax: ~a" stx)))))

(define parse-term (parse parse-extra))
(define parse-val (parse-value parse-term))

(define (parse-bit senv head tail)
  (match tail
    ((list 0) (t-value (v-bit (b-0))))
    ((list 1) (t-value (v-bit (b-1))))
    (_        (error (format "invalid bit: ~a" `(,head . ,tail))))))

(define (parse-if senv head tail)
  (define (pthunk stx) ((parse-thunk parse-term) senv stx))
  (match tail
    ((list cnd tcase fcase)
     (t-apply (t-unpair (t-apply (std0 'boolean->bit) (parse-term senv cnd))
                        (t-value (v-pair (pthunk tcase) (pthunk fcase))))
              (t-value (v-unit))))
    (_ (error (format "invalid if: ~a" `(,head . ,tail))))))

(record symbol-table symbol->value count->symbol)
(define symbol-table-empty (symbol-table (hash) (hash)))
(def (symbol-table-get st sym)
  (symbol-table s->v c->s) = st
  (match (hash-get s->v sym)
    ((nothing) (lets count = (hash-count s->v)
                     c->s = (hash-set c->s count sym)
                     val = (nat->symbol count)
                     s->v = (hash-set s->v sym val)
                     (values (symbol-table s->v c->s) val)))
    ((just val) (values st val))))
(define *symbol-table* (box symbol-table-empty))
(def ((boxed-symbol-table-get bx) sym)
  (values st result) = (symbol-table-get (unbox bx) sym)
  _ = (set-box! bx st)
  result)
(define symbol->value! (boxed-symbol-table-get *symbol-table*))

(define (parse-quoted senv stx)
  (match stx
    (`(,hd . ,tl)
      (t-apply (t-apply (std0 'cons) (parse-quoted senv hd))
               (parse-quoted senv tl)))
    ('() (std0 'nil))
    ((? symbol?) (symbol->value! stx))
    (_ (parse-extra senv stx))))

(define (parse-quote senv head tail)
  (match tail
    ((list stx) (parse-quoted senv stx))
    (_ (error (format "invalid quote: ~a" `(,head . ,tail))))))

(define unsafe1-specials `(
  (quote      . ,parse-quote)
  (lambda     . ,(parse-lambda parse-term))
  (bit        . ,parse-bit)
  (pair       . ,(parse-pair parse-val))
  (unpair     . ,(parse-unpair parse-term))
  (if         . ,parse-if)
  (let        . ,(parse-let parse-term))
  (let*       . ,(parse-let* parse-term))
  ))

(define unsafe1-senv-empty (senv-new unsafe1-specials))
(define unsafe1-parse (curry parse-term unsafe1-senv-empty))

(module+ test
  (check-equal?
    (denote (unsafe1-parse '((lambda (a b) (pair a b))
                              (if #t 5 7) (if #f (bit 0) (bit 1)))))
    `(,(denote (unsafe0-parse (int->integer 5))) . 1))
  (check-equal?
    (denote (unsafe1-parse ''((#t #f) (0 1))))
    (denote (std0-apply '(lambda (nil cons true false zero one)
                            (cons (cons true (cons false nil))
                                  (cons (cons zero (cons one nil)) nil)))
                         'nil 'cons 'true 'false 'zero 'positive-one)))
  (check-equal?
    (denote (unsafe1-parse ''(a b c b a c)))
    (forf
      result = (denote
                  (std0-apply
                    '(lambda (nil cons a b c)
                       (cons a (cons b (cons c (cons
                                                 b (cons a (cons c nil)))))))
                    'nil 'cons))
      sym <- '(a b c)
      (result (denote (symbol->value! sym)))))
  )

; bindings must not redefine 'pair'
(def (unsafe1-module std0-imports bindings)
  (list import-params import-args) =
  (zip-default '(() ())
    (forl import <- std0-imports
          (match import
            ((? symbol?)               (list import (std0 import)))
            ((list name original-name) (list name (std0 original-name)))
            (_ (error (format "invalid import: ~a" import))))))
  names = (forl (list name expr) <- bindings name)
  body = (foldr (lambda (name acc) `(pair ,name ,acc)) '() names)
  pre-module = (unsafe1-parse `(lambda ,import-params (let* ,bindings ,body)))
  prog = (build-apply pre-module import-args)
  vals = (tuple0->list (t-value-v (substitute-full (step-complete prog))))
  assocs = (forl name <- names val <- vals (cons name val))
  (make-immutable-hash assocs))

(module+ test
  (lets
    mod = (unsafe1-module
            '(pcons (rest ptail) head tail (eq? symbol=?))
            '((datum (pcons () '(a b a)))
              (d0 (head (rest datum)))
              (d1 (head (tail (rest datum))))
              (d2 (head (tail (tail (rest datum)))))
              (eq01? (eq? d0 d1))
              (eq02? (eq? d0 d2))))
    export = (compose t-value (curry hash-ref mod))
    (begin
      (check-equal?
        (denote (export 'eq01?))
        (denote (std0 'false)))
      (check-equal?
        (denote (export 'eq02?))
        (denote (std0 'true)))))
  )
