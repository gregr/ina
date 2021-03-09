;; recognize symbol characters that must be escaped when writing

;; TODO: indent? pretty printing? optional formatting styles?
;; quote/abbreviations, padding, packing, numeric formats/radixes/rounding
;; fine-grained writing styles? express write in terms of richer documents?

(define (write v out)
  (define punc? (cset "\\\"#'(),;[]`{}"))
  (define (bad-symbol? c) (or (unicode-space? c) (punc? c)))
  (define (pr s) (out 'put* s))
  (define (write* v)
    (write (car v) out)
    (cond ((pair? (cdr v))       (pr " ")   (write* (cdr v)))
          ((not (null? (cdr v))) (pr " . ") (write (cdr v) out))))
  (define (bracket l r v) (pr l) (write* v) (pr r))
  (cond ((eq? v #t)  (pr "#t"))
        ((eq? v #f)  (pr "#f"))
        ((null? v)   (pr "()"))
        ((number? v) (write-number v out))
        ((symbol? v)
         (define s (symbol->string v))
         (cond ((eq? v '|.|)       (pr "|.|"))
               ((string->number s) (pr "|") (pr s) (pr "|"))
               (else (define cs   (string->unicodes s))
                     (define bad? (ormap bad-symbol? cs))
                     (when bad? (pr "|"))
                     (for-each
                       (lambda (c)
                         (cond ((eqv? c (char "|")) (when bad? (pr "|"))
                                                    (pr "\\|")
                                                    (when bad? (pr "|")))
                               (else (pr (list->string (unicode->utf8 c))))))
                       cs)
                     (when bad? (pr "|")))))
        ((string? v)
         (pr "\"")
         (for-each
           (lambda (c)
             (pr (case/char c ("\"" "\\\"") ("\\" "\\\\") ("\a" "\\a")
                   ("\b" "\\b") ("\e" "\\e") ("\f" "\\f") ("\n" "\\n")
                   ("\r" "\\r") ("\t" "\\t") ("\v" "\\v")
                   (else (if (or (unicode-control? c) (unicode-vspace? c))
                           (string-append "\\u" (number->string c) ";")
                           (list->string (list c)))))))
           (string->unicodes v))
         (pr "\""))
        ((pair?      v) (bracket "(" ")" v))
        ((vector?    v) (cond ((eqv? (vector-length  v) 0) (pr "#()"))
                              (else (bracket "#(" ")"  (vector->list  v)))))
        ((mvector?   v) (cond ((eqv? (mvector-length v) 0) (pr "#m()"))
                              (else (bracket "#m(" ")" (mvector->list v)))))
        ((procedure? v) (pr "#<procedure>"))))

;; TODO: reduce dependency on exact->inexact and inexact->exact
(define (write-number n out)
  (define (pr s)       (out 'put* s))
  (define (pr-digit d) (out 'put (+ d (char "0"))))
  (define (pr-nat n)
    (if (eqv? n 0) (pr "0")
      (let loop ((n n) (ds '()))
        (if (> n 0) (loop (quotient n 10) (cons (remainder n 10) ds))
          (for-each pr-digit ds)))))
  (define (pr-int n) (cond ((< n 0) (pr "-") (pr-nat (- n)))
                           (else             (pr-nat    n))))
  (define (larger-log10 n)
    (if (< n 1) 0
      (let loop ((e 1) (p 10) (e+ 1) (p+ 10) (prev '()))
        (if (< n p+)
          (let loop ((e e) (p p) (e.best e+) (p.best p+) (prev prev))
            (if (null? prev) e.best
              (let ((e.0 (caar prev)) (p.0 (cdar prev)) (prev (cdr prev)))
                (define e+ (+ e e.0))
                (define p+ (* p p.0))
                (if (< n p+)
                  (loop e  p  e+     p+     prev)
                  (loop e+ p+ e.best p.best prev)))))
          (loop e+ p+ (+ e+ e+) (* p+ p+) (cons (cons e p) prev))))))
  (define (pr-float n.inexact)
    (define n (inexact->exact n.inexact))
    (define (finish rdigits e)
      (define ds (reverse rdigits))
      (if (and (eqv? 0 (car ds)) (< 0 e))
        (finish.1 (cdr ds) (- e 1))
        (finish.1      ds     e)))
    (define (finish.1 digits e)
      (define (pr-dec digits e?)
        (pr-digit (car digits))
        (cond ((null? (cdr digits)) (unless e? (pr ".") (pr-digit 0)))
              (else                 (pr ".") (for-each pr-digit (cdr digits)))))
      (define (pr-exp digits e) (pr-dec digits #t) (pr "e") (pr-int e))
      (cond ((<= #e1e10 n       ) (pr-exp digits e))
            ((<  0      n #e1e-3) (let loop ((digits digits) (e e))
                                    (if (eqv? 0 (car digits))
                                      (loop (cdr digits) (- e 1))
                                      (pr-exp digits e))))
            (else (let loop ((ds digits) (e e))
                    (define digits (if (null? ds) '(0) ds))
                    (cond ((<= e 0) (pr-dec digits #f))
                          (else     (pr-digit (car digits))
                                    (loop (cdr digits) (- e 1))))))))
    (define e   (larger-log10 n))
    (define e10 (expt 10 e))
    (define m   (/ n e10))
    (let loop ((m m) (approx 0) (scale e10) (rdigits '()))
      (define   d   (truncate m))
      (define a+d   (+ approx (*    d    scale)))
      (define a+d+1 (+ approx (* (+ d 1) scale)))
      (cond ((eqv? (exact->inexact a+d  ) n.inexact)
             (finish (cons    d    rdigits) e))
            ((eqv? (exact->inexact a+d+1) n.inexact)
             (finish (cons (+ d 1) rdigits) e))
            (else (loop (* (- m d) 10) a+d (/ scale 10) (cons d rdigits))))))
  (cond ((not (real? n)) (define i (imag-part n))
                         (write-number (real-part n) out)
                         (when (and (<= 0 i) (not (eqv? i +inf.0))) (pr "+"))
                         (write-number i out) (pr "i"))
        ((eqv? n +inf.0) (pr "+inf.0"))
        ((eqv? n -inf.0) (pr "-inf.0"))
        ((eqv? n +nan.0) (pr "+nan.0"))
        ((inexact? n)    (when (or (< n 0) (equal? n -0.0)) (pr "-"))
                         (pr-float     (abs n)))
        ((integer? n)    (pr-int            n))
        (else            (pr-int (numerator n)) (pr "/") (pr-nat (denominator n)))))

(define (number->string n) (let ((out (port:string:output)))
                             (write-number n out)
                             (out 'string)))

;; TODO: format, fprintf
