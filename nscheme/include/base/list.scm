(define (list . x*)   x*)

(splicing-local
  ((define (append2 x* y)
     (cond ((null? x*) y)
           (else       (cons (car x*) (append2 (cdr x*) y))))))
  (define append
    (case-lambda
      (()          '())
      ((x* . rest) (let loop ((x* x*) (rest rest))
                     (cond ((null? rest) x*)
                           (else         (append2 x* (loop (car rest) (cdr rest))))))))))

(define (improper-list->list x*)
  (cond ((null? x*) '())
        ((pair? x*) (cons (car x*) (improper-list->list (cdr x*))))
        (else       (list x*))))

(define (improper-list-map f x*)
  (let loop ((x* x*))
    (cond ((null? x*) '())
          ((pair? x*) (cons (f (car x*)) (loop (cdr x*))))
          (else       (f x*)))))

(define (memp ? x*)
  (let loop ((x* x*))
    (and (not (null? x*))
         (cond ((? (car x*)) x*)
               (else         (loop (cdr x*)))))))

(define (mem/= =? y x*) (memp (lambda (x) (=? x y)) x*))

(define (memq   y x*) (mem/= eq?    y x*))
(define (memv   y x*) (mem/= eqv?   y x*))
(define (member y x*) (mem/= equal? y x*))

(define (rem1p ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cdr x*))
          (else         (cons (car x*) (loop (cdr x*)))))))

(define (rem/= =? y x*) (rem1p (lambda (x) (=? x y)) x*))

(define (remq1   y x*) (rem/= eq?    y x*))
(define (remv1   y x*) (rem/= eqv?   y x*))
(define (remove1 y x*) (rem/= equal? y x*))

(define (iota n)
  (unless (and (integer? n) (<= 0 n)) (error "not a nonnegative integer" n))
  (let loop ((i 0))
    (cond ((= i n) '())
          (else    (cons i (loop (+ i 1)))))))

(define (assp ? alist)
  (let loop ((alist alist))
    (and (not (null? alist))
         (let ((kv (car alist)))
           (cond ((? (car kv)) kv)
                 (else         (loop (cdr alist))))))))

(define (assoc/= =? key alist) (assp (lambda (k) (=? k key)) alist))

(define (assq  key alist) (assoc/= eq?    key alist))
(define (assv  key alist) (assoc/= eqv?   key alist))
(define (assoc key alist) (assoc/= equal? key alist))

(define (plist->alist kvs) (cond ((null? kvs) '())
                                 (else        (cons (cons (car kvs) (cadr kvs))
                                                    (plist->alist (cddr kvs))))))

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

(define (length x*)
  (let loop ((x* x*) (l 0))
    (cond ((null? x*) l)
          (else (loop (cdr x*) (+ l 1))))))

(define (list-tail x* i)
  (unless (<= 0 i) (error "not a nonnegative integer" i))
  (let loop ((x* x*) (i i))
    (cond ((= i 0) x*)
          (else    (loop (cdr x*) (- i 1))))))

(define (list-ref x* i) (car (list-tail x* i)))

(define (cons* x . x*)
  (let loop ((x x) (x* x*))
    (cond ((null? x*) x)
          (else       (cons x (loop (car x*) (cdr x*)))))))

(define (reverse x*)
  (let loop ((x* x*) (acc '()))
    (cond ((null? x*) acc)
          (else       (loop (cdr x*) (cons (car x*) acc))))))

(define (filter ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cons (car x*) (loop (cdr x*))))
          (else         (loop (cdr x*))))))

(define (filter-not ? x*) (filter (lambda (x) (not (? x))) x*))
(define (true* x*)        (filter (lambda (x) x)           x*))
(define (null*? x*)       (let loop ((x* x*))
                            (or (null? x*) (and (null? (car x*)) (loop (cdr x*))))))

(splicing-local
  ((define (map1 f x*)
     (let loop ((x* x*))
       (cond ((null? x*) '())
             (else       (cons (f (car x*)) (loop (cdr x*)))))))
   (define (andmap1 f x*)
     (or (null? x*)
         (let loop ((x (car x*)) (x* (cdr x*)))
           (cond ((null? x*) (f x))
                 (else       (and (f x) (loop (car x*) (cdr x*)))))))))

  (define map
    (case-lambda
      ((f x*)       (map1 f x*))
      ((f y* . y**) (let loop ((x* y*) (x** y**))
                      (cond ((null? x*) (unless (andmap1 null? x**)
                                          (error "lists of different length"
                                                 (cons (length y*) (map1 length y**))))
                                        '())
                            (else (cons (apply f (car x*) (map1 car x**))
                                        (loop (cdr x*) (map1 cdr x**)))))))))

  (define for-each
    (case-lambda
      ((f x*)       (let loop ((x* x*)) (unless (null? x*) (f (car x*)) (loop (cdr x*)))))
      ((f y* . y**) (let loop ((x* y*) (x** y**))
                      (cond ((null? x*) (unless (andmap1 null? x**)
                                          (error "lists of different length"
                                                 (cons (length y*) (map1 length y**)))))
                            (else       (apply f (car x*) (map1 car x**))
                                        (loop (cdr x*) (map1 cdr x**))))))))

  (define andmap
    (case-lambda
      ((f x*)       (andmap1 f x*))
      ((f y* . y**) (cond ((null? y*) (unless (andmap1 null? y**)
                                        (error "lists of different length"
                                               (cons (length y*) (map1 length y**))))
                                      #t)
                          (else (let loop ((x (car y*)) (x* (cdr y*)) (x** y**))
                                  (cond ((null? x*) (unless (andmap1 null? (map1 cdr x**))
                                                      (error "lists of different length"
                                                             (cons (length y*) (map1 length y**))))
                                                    (apply f x (map1 car x**)))
                                        (else (and (apply f x (map1 car x**))
                                                   (loop (car x*) (cdr x*) (map1 cdr x**)))))))))))

  (define ormap
    (case-lambda
      ((f x*)       (and (not (null? x*))
                         (let loop ((x (car x*)) (x* (cdr x*)))
                           (cond ((null? x*) (f x))
                                 (else (or (f x) (loop (car x*) (cdr x*))))))))
      ((f y* . y**) (cond ((null? y*) (unless (andmap1 null? y**)
                                        (error "lists of different length"
                                               (cons (length y*) (map1 length y**))))
                                      #f)
                          (else (let loop ((x (car y*)) (x* (cdr y*)) (x** y**))
                                  (cond ((null? x*) (unless (andmap1 null? (map1 cdr x**))
                                                      (error "lists of different length"
                                                             (cons (length y*) (map1 length y**))))
                                                    (apply f x (map1 car x**)))
                                        (else (or (apply f x (map1 car x**))
                                                  (loop (car x*) (cdr x*) (map1 cdr x**))))))))))))

