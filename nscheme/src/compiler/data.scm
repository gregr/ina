(define (mloc w b d i s) (vector 'mloc w b d i s))
(define (mloc? x) (and (vector? x) (= (vector-length x) 6) (eqv? (vector-ref x 0) 'mloc)))
(define (mloc-width x) (vector-ref x 1))
(define (mloc-base  x) (vector-ref x 2))
(define (mloc-disp  x) (vector-ref x 3))
(define (mloc-index x) (vector-ref x 4))
(define (mloc-shift x) (vector-ref x 5))

(define (make-relocation) (mvector #f))
(define (relocation? x) (and (mvector? x) (= (mvector-length x) 1)))
(define (relocation-ref x) (mvector-ref x 0))
(define (relocation-set! x offset) (mvector-set! x 0 offset))

(splicing-let ((2^63 #x8000000000000000) (2^64 #x10000000000000000))
  (define s64-min (- 2^63))
  (define s64-max (- 2^63 1))
  (define u64-max (- 2^64 1))
  (define (s64 x) (let ((x (integer-floor-mod x 2^64))) (if (< x 2^63) x (- x 2^64))))
  (define (u64 x) (integer-floor-mod x 2^64)))

(splicing-local
  ((define rtd.uvar  (make-rtd 'uvar 3 #t 'uvar))  ; TODO: analysis annotations
   (define make-uvar (rtd-constructor rtd.uvar))
   (define uvar-ref  (rtd-accessor    rtd.uvar))
   (define uvar-set! (rtd-mutator     rtd.uvar)))
  (define uvar? (rtd-predicate rtd.uvar))
  (define (uvar name) (make-uvar name #f #f))
  (define (uvar-name x) (uvar-ref x 0))
  (define (uvar-uid  x) (uvar-ref x 1))
  (define (uvar-note x) (uvar-ref x 2))
  (define (set-uvar-uid!  x uid)  (uvar-set! x 1 uid))
  (define (set-uvar-note! x note) (uvar-set! x 2 note))
  (define (uvar->symbol x)
    (let ((name (uvar-name x)) (uid (uvar-uid x)))
      (string->symbol (string-append (if name (symbol->string name) "_") "."
                                     (if uid (number->string uid) "_"))))))

;; TODO: arity is not specific enough
(splicing-local
  ((define rtd.primop (make-rtd 'primop 2 #f 'primop))  ; TODO: behavioral properties and open code
   (define primop-ref (rtd-accessor rtd.primop)))
  (define primop  (rtd-constructor rtd.primop))
  (define primop? (rtd-predicate rtd.primop))
  (define (primop-name  x) (primop-ref x 0))
  (define (primop-arity x) (primop-ref x 1)))
