(provide tagged-vector? tagged-vector?! test!)

(define (tagged-vector? tag field-names d)
  (define len (+ 1 (length field-names)))
  (and (vector? d) (= len (vector-length d))
       (equal? tag (vector-ref d 0))
       (map (lambda (name i) (cons name (vector-ref d i)))
            field-names (range 1 len))))

(define (tagged-vector?! tag fnames d)
  (or (tagged-vector? tag fnames d)
      (error '"invalid tagged vector:" (cons tag fnames) d)))

(define (test! test)
  (define example:tag         'example)
  (define (example x y)       (vector example:tag x y))
  (define (example? d)        (tagged-vector? example:tag '(x y) d))
  (define (example?! d)       (tagged-vector?! example:tag '(x y) d))
  (define (example-x e)       (import/apply (import (x) x) (example?! e)))
  (define (example-x-set e x) (example?! e) (vector-set e 1 x))

  (test 'example-1
    (example? (example 1 2))
    '((x . 1) (y . 2)))

  (test 'example-2
    (example? (vector 'non-example 1 2))
    #f)

  (test 'example-3
    (example? #t)
    #f)

  (test 'example-4
    (example-x (example 2 3))
    2)

  (test 'example-5
    (example-x (example-x-set (example 2 3) 5))
    5))
