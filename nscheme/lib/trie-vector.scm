((provide tvec:empty tvec->list list->tvec
          tvec-length tvec-ref tvec-set tvec-push tvec-pop)
 (require test))

;; TODO: standardize these.
(define (vector-push v x)
  (define len (vector-length v))
  (define mv (make-mvector (+ len 1) x))
  (let loop ((i (- len 1)))
    (when (<= 0 i) (mvector-set! mv i (vector-ref v i)) (loop (- i 1))))
  (mvector->vector mv))
(define (vector-pop v)
  (define len (vector-length v))
  (define mv (make-mvector (- len 1) #t))
  (let loop ((i (- len 2)))
    (when (<= 0 i) (mvector-set! mv i (vector-ref v i)) (loop (- i 1))))
  (mvector->vector mv))

(define shift-size 4)
(define branch-size (arithmetic-shift 1 shift-size))
(define local-mask (- branch-size 1))
(define rshift-size (* -1 shift-size))
(define (local-i i)   (bitwise-and i local-mask))
(define (shift-i i)   (arithmetic-shift i rshift-size))
(define (unshift-i i) (arithmetic-shift i shift-size))
(define (full-shift sz)
  (let loop ((i (shift-i sz))) (if (= 0 (bitwise-and local-mask i))
                                 (+ shift-size (loop (shift-i i))) 0)))
(define (depth-shift sz)
  (let loop ((i (- sz 1))) (if (< i branch-size) 0
                             (+ shift-size (loop (shift-i i))))))
(define (tvec len trie tail) (vector len trie tail))
(define (tvec-length t)      (vector-ref t 0))
(define (tvec-trie t)        (vector-ref t 1))
(define (tvec-tail t)        (vector-ref t 2))

(define tvec:empty (tvec 0 #t '#()))
(define (tvec->list t)
  (let loop ((t t) (xs '()))
    (define len (tvec-length t))
    (if (= 0 len) xs (loop (tvec-pop t) (cons (tvec-ref t (- len 1)) xs)))))
(define (list->tvec xs) (foldl (lambda (x t) (tvec-push t x)) tvec:empty xs))
(define (tvec-ref t i)
  (define len         (tvec-length t))
  (define tail        (tvec-tail t))
  (define tail-length (vector-length tail))
  (define tail-offset (- len tail-length))
  (if (<= tail-offset i) (vector-ref tail (- i tail-offset))
    (let loop ((t (tvec-trie t)) (dsh (depth-shift tail-offset)))
      (define j (arithmetic-shift i (- dsh)))
      (define b (vector-ref t (bitwise-and local-mask j)))
      (if (= 0 dsh) b (loop b (- dsh shift-size))))))
(define (tvec-set t i x)
  (define trie        (tvec-trie t))
  (define len         (tvec-length t))
  (define tail        (tvec-tail t))
  (define tail-length (vector-length tail))
  (define tail-offset (- len tail-length))
  (if (<= tail-offset i) (tvec len trie (vector-set tail (- i tail-offset) x))
    (tvec len (let loop ((t trie) (dsh (depth-shift tail-offset)))
                (define j (bitwise-and local-mask
                                       (arithmetic-shift i (- dsh))))
                (vector-set t j (if (= 0 dsh) x
                                  (loop (vector-ref t j) (- dsh shift-size)))))
          tail)))
(define (tvec-push t x)
  (define trie        (tvec-trie t))
  (define len         (tvec-length t))
  (define tail        (tvec-tail t))
  (define tail-length (vector-length tail))
  (define tail-offset (- len tail-length))
  (if (< tail-length branch-size)
    (tvec (+ len 1) (tvec-trie t) (vector-push tail x))
    (let* ((dsh (depth-shift tail-offset))
           (fsh (and (< 0 tail-offset) (full-shift tail-offset)))
           (new (and fsh (let loop ((i fsh))
                           (if (= 0 i) tail
                             (vector (loop (- i shift-size)))))))
           (trie (cond ((= 0 tail-offset) tail)
                       ((= dsh fsh)       (vector trie new))
                       (#t (let loop ((t trie) (i (- dsh shift-size)))
                             (define j (- (vector-length t) 1))
                             (if (= i fsh) (vector-push t new)
                               (vector-set t j (loop (vector-ref t j)
                                                     (- i shift-size)))))))))
      (tvec (+ len 1) trie (vector x)))))
(define (tvec-pop t)
  (define trie        (tvec-trie t))
  (define len         (tvec-length t))
  (define tail        (tvec-tail t))
  (define tail-length (vector-length tail))
  (define tail-offset (- len tail-length))
  (define dsh         (depth-shift tail-offset))
  (define dsh0        (depth-shift (- tail-offset branch-size)))
  (cond ((< 1 tail-length)         (tvec (- len 1) trie (vector-pop tail)))
        ((= 1 len)                 tvec:empty)
        ((= (+ branch-size 1) len) (tvec branch-size #t trie))
        ((< dsh0 dsh) (tvec (- len 1) (vector-ref trie 0)
                            (let loop ((t (vector-ref trie 1)) (i dsh0))
                              (if (= 0 i) t
                                (loop (vector-ref t 0) (- i shift-size))))))
        (#t (define fsh (full-shift (- tail-offset branch-size)))
         (define new-tail #t)
         (define new-trie
           (let loop ((t trie) (i (- dsh fsh shift-size)))
             (define j (- (vector-length t) 1))
             (cond ((= i 0) (let loop ((t (vector-ref t j)) (i fsh))
                              (if (= 0 i) (set! new-tail t)
                                (loop (vector-ref t 0) (- i shift-size))))
                            (vector-pop t))
                   (#t (vector-set t j (loop (vector-ref t j)
                                             (- i shift-size)))))))
         (tvec (- len 1) new-trie new-tail))))

;; TODO: define batch/interval/slice operators:
;; tvec-ref*, tvec-set*, tvec-push*, tvec-pop*
;; For efficiency, redefine single-element operators in terms of these.

(when test
  (test 'tvec-identity-1
    (tvec->list (list->tvec (range (+ 1 branch-size))))
    (range (+ 1 branch-size)))
  (test 'tvec-identity-2
    (tvec->list (list->tvec (range (+ 1 (* 2 branch-size)))))
    (range (+ 1 (* 2 branch-size))))
  (test 'tvec-identity-3
    (tvec->list (list->tvec (range (+ 1 branch-size (* branch-size branch-size)))))
    (range (+ 1 branch-size (* branch-size branch-size))))
  ;; TODO: improve performance.  This test takes half a second with shift-size=4.
  ;(test 'tvec-identity-4
    ;(tvec->list
      ;(list->tvec (range (+ 1 branch-size
                            ;(* branch-size branch-size branch-size)))))
    ;(range (+ 1 branch-size (* branch-size branch-size branch-size))))

  (test 'tvec-ref-1
    (tvec-ref (list->tvec (range (+ 1 branch-size))) (- branch-size 1))
    (- branch-size 1))
  (test 'tvec-ref-2
    (tvec-ref (list->tvec (range (+ 1 branch-size))) branch-size)
    branch-size )
  (test 'tvec-ref-3
    (tvec-ref (list->tvec (range (+ 1 (* 2 branch-size))))
              (- (* 2 branch-size) 1))
    (- (* 2 branch-size) 1))
  (test 'tvec-ref-4
    (tvec-ref (list->tvec (range (+ 1 (* 2 branch-size)))) (* 2 branch-size))
    (* 2 branch-size))
  (test 'tvec-ref-5
    (tvec-ref (list->tvec (range (+ 1 (* branch-size branch-size))))
              (* branch-size branch-size))
    (* branch-size branch-size))
  (test 'tvec-ref-6
    (tvec-ref
      (list->tvec (range (+ 1 (* branch-size branch-size branch-size))))
      (- (* branch-size branch-size) 1))
    (- (* branch-size branch-size) 1))

  (test 'tvec-set-1
    (tvec-set (list->tvec (range (+ 1 branch-size))) (- branch-size 1) 'x)
    (vector (+ 1 branch-size)
            (vector-set (list->vector (range branch-size))
                        (- branch-size 1) 'x) (vector branch-size)))
  (test 'tvec-set-2
    (tvec-set (list->tvec (range (+ 1 branch-size))) branch-size 'x)
    (vector (+ 1 branch-size) (list->vector (range branch-size)) (vector 'x)))
  (test 'tvec-set-3
    (tvec-set (list->tvec (range (+ 1 (* 2 branch-size))))
              (- (* 2 branch-size) 1) 'x)
    (vector (+ 1 (* 2 branch-size))
            (vector (list->vector (range branch-size))
                    (vector-set (list->vector
                                  (drop (range (* 2 branch-size)) branch-size))
                                (- branch-size 1) 'x))
            (vector (* 2 branch-size))))
  (test 'tvec-set-4
    (tvec-set
      (list->tvec (range (+ 1 (* 2 branch-size)))) (* 2 branch-size) 'x)
    (vector (+ 1 (* 2 branch-size))
            (vector (list->vector (range branch-size))
                    (list->vector (drop (range (* 2 branch-size))
                                        branch-size)))
            (vector 'x)))
  (test 'tvec-set-5
    (tvec->list
      (tvec-set (list->tvec (range (+ 1 (* branch-size branch-size))))
                (* branch-size branch-size) 'x))
    (append (range (* branch-size branch-size)) '(x)))
  (test 'tvec-set-6
    (tvec->list
      (tvec-set (list->tvec (range (+ 1 (* branch-size branch-size))))
                (- (* branch-size branch-size) 1) 'x))
    (append (range (- (* branch-size branch-size) 1))
            (list 'x (* branch-size branch-size))))
  )
