(provide
  assoc-empty
  assoc-ref
  assoc-set
  assoc-remove
  assoc-remove*
  assoc-simplify
  test!)

(define assoc-empty '())

(define (assoc-ref d k default)
  (define rib (assoc k d))
  (if rib (cdr rib) default))

(define (assoc-set d k v) (cons (cons k v) d))

(define (assoc-remove* d k*)
  (cond ((null? d)            assoc-empty)
        ((member (caar d) k*) (assoc-remove* (cdr d) k*))
        (else                 (cons (car d) (assoc-remove* (cdr d) k*)))))

(define (assoc-remove d k) (assoc-remove* d (list k)))

(define (assoc-simplify d)
  (let loop ((d d) (k* '()))
    (cond ((null? d)            assoc-empty)
          ((member (caar d) k*) (loop (cdr d) k*))
          (else (cons (car d) (loop (cdr d) (cons (caar d) k*)))))))

(define (test! test)
  (test 'assoc-1
    (assoc-ref (assoc-set assoc-empty 'a 1) 'a #f)
    1)
  (test 'assoc-2
    (assoc-ref (assoc-set (assoc-set assoc-empty 'a 1) 'a 2) 'a #f)
    2)
  (test 'assoc-3
    (assoc-remove
      (assoc-set (assoc-set (assoc-set (assoc-set assoc-empty
                                                  'a 1) 'b 2) 'a 3) 'c 4)
      'a)
    '((c . 4) (b . 2)))
  (test 'assoc-4
    (assoc-remove
      (assoc-set (assoc-set (assoc-set (assoc-set assoc-empty
                                                  'a 1) 'b 2) 'a 3) 'c 4)
      'b)
    '((c . 4) (a . 3) (a . 1)))
  (test 'assoc-5
    (assoc-remove*
      (assoc-set (assoc-set (assoc-set (assoc-set assoc-empty 'a 1) 'b 2)
                            'a 3) 'c 4)
      '(c b))
    '((a . 3) (a . 1)))
  (test 'assoc-6
    (assoc-simplify
      (assoc-set (assoc-set (assoc-set (assoc-set assoc-empty
                                                  'a 1) 'b 2) 'a 3) 'c 4))
    '((c . 4) (a . 3) (b . 2))))
