((provide compare max/any min/any)
 (require test))

(define (typecode d)
  (cond ((null? d)      0)
        ((boolean? d)   1)
        ((number? d)    2)
        ((string? d)    3)
        ((pair? d)      4)
        ((vector? d)    5)
        ((mvector? d)   6)
        ((procedure? d) 7)
        (#t '"no typecode for type:" d)))
(define (typecode->compare t)
  (define (? i) (equal? t i))
  (cond ((? 0) compare-null)
        ((? 1) compare-boolean)
        ((? 2) compare-number)
        ((? 3) compare-string)
        ((? 4) compare-pair)
        ((? 5) compare-vector)
        ;; TODO: is it more useful to treat these as always equal?
        ;; Then they could at least be sorted alongside other values.
        ((? 6) (error '"cannot compare mvectors"))
        ((? 7) (error '"cannot compare procedures"))
        (#t '"invalid typecode:" t)))

(define (compare-typecode ta tb lt eq gt)
  (cond ((< ta tb) lt) ((> ta tb) gt) (#t eq)))
(define (compare-null a b lt eq gt) eq)
(define (compare-boolean a b lt eq gt) (if a (if b eq gt) (if b lt eq)))
(define (compare-number a b lt eq gt)
  (cond ((< a b) lt) ((> a b) gt) (#t eq)))
(define (compare-string a b lt eq gt)
  (cond ((string<? a b) lt) ((string>? a b) gt) (#t eq)))
(define (compare-pair a b lt eq gt)
  ((compare (car a) (car b) (lambda () lt)
            (lambda () (compare (cdr a) (cdr b) lt eq gt))
            (lambda () gt))))
(define (compare-vector a b lt eq gt)
  (define la (vector-length a)) (define lb (vector-length b))
  (cond ((< la lb) lt)
        ((> la lb) gt)
        (#t (compare-pair (vector->list a) (vector->list b) lt eq gt))))
(define (compare a b lt eq gt)
  (define ta (typecode a)) (define tb (typecode b))
  ((compare-typecode ta tb (lambda () lt)
                     (lambda () ((typecode->compare ta) a b lt eq gt))
                     (lambda () gt))))

(define (max/any . xs)
  (let loop ((x (car xs)) (xs (cdr xs)))
    (if (null? xs) x (loop (compare x (car xs) (car xs) x x) (cdr xs)))))
(define (min/any . xs)
  (let loop ((x (car xs)) (xs (cdr xs)))
    (if (null? xs) x (loop (compare x (car xs) x x (car xs)) (cdr xs)))))

(when test
  (test 'compare-1
    (compare 1 2 'lt 'eq 'gt)
    'lt)
  (test 'compare-2
    (compare 3 2 'lt 'eq 'gt)
    'gt)
  (test 'compare-3
    (compare 2 2 'lt 'eq 'gt)
    'eq)
  (test 'compare-4
    (compare #t #f 'lt 'eq 'gt)
    'gt)
  (test 'compare-5
    (compare #t #t 'lt 'eq 'gt)
    'eq)
  (test 'compare-6
    (compare #f #t 'lt 'eq 'gt)
    'lt)
  (test 'compare-7
    (compare #f (vector 1 2) 'lt 'eq 'gt)
    'lt)
  (test 'compare-8
    (compare (cons 1 2) (vector 1 2) 'lt 'eq 'gt)
    'lt)
  (test 'compare-9
    (compare (vector 1 2) (cons 1 2) 'lt 'eq 'gt)
    'gt)
  (test 'compare-10
    (compare (vector 1 2) (vector 1 2) 'lt 'eq 'gt)
    'eq)
  (test 'compare-11
    (compare (vector 1 2) (vector 2 2) 'lt 'eq 'gt)
    'lt)
  (test 'compare-12
    (compare (vector 1 2) (vector 1 3) 'lt 'eq 'gt)
    'lt)
  (test 'compare-13
    (compare (vector 1 22) (vector 1 3) 'lt 'eq 'gt)
    'gt)
  (test 'compare-14
    (compare (vector 1 22) '() 'lt 'eq 'gt)
    'gt)
  (test 'compare-15
    (compare '() 'ok 'lt 'eq 'gt)
    'lt)

  (test 'max-1
    (max/any 3 8 #t 'x #f)
    'x)
  (test 'max-2
    (max/any 3 8 #t 'x #f (cons 88 7))
    (cons 88 7))
  (test 'max-3
    (max/any 3 8 #t 'x (vector 2) #f (cons 88 7))
    (vector 2))

  (test 'min-1
    (min/any 3 8 'x (vector 2) (cons 88 7))
    3)
  (test 'min-2
    (min/any 3 8 #t 'x (vector 2) (cons 88 7))
    #t)
  (test 'min-3
    (min/any 3 8 #t 'x (vector 2) #f (cons 88 7))
    #f))
