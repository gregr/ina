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
(define ((mem/= =?) y x*) (memp (lambda (x) (=? x y)) x*))
(define memq   (mem/= eq?))
(define memv   (mem/= eqv?))
(define member (mem/= equal?))

(define (pmemp ? x*)
  (let loop ((x* x*))
    (and (not (null? x*))
         (cond ((? (car x*)) x*)
               (else         (loop (cddr x*)))))))
(define ((pmem/= =?) y x*) (pmemp (lambda (x) (=? x y)) x*))
(define pmemq   (pmem/= eq?))
(define pmemv   (pmem/= eqv?))
(define pmember (pmem/= equal?))

(define (rem1p ? x*)
  (let loop ((x* x*))
    (cond ((null? x*)   '())
          ((? (car x*)) (cdr x*))
          (else         (cons (car x*) (loop (cdr x*)))))))
(define ((rem/= =?) y x*) (rem1p (lambda (x) (=? x y)) x*))
(define remq1   (rem/= eq?))
(define remv1   (rem/= eqv?))
(define remove1 (rem/= equal?))

(define (assp ? alist)
  (let loop ((alist alist))
    (and (not (null? alist))
         (let ((kv (car alist)))
           (cond ((? (car kv)) kv)
                 (else         (loop (cdr alist))))))))
(define ((assoc/= =?) key alist) (assp (lambda (k) (=? k key)) alist))
(define assq  (assoc/= eq?))
(define assv  (assoc/= eqv?))
(define assoc (assoc/= equal?))

(define (plist->alist kvs) (cond ((null? kvs) '())
                                 (else        (cons (cons (car kvs) (cadr kvs))
                                                    (plist->alist (cddr kvs))))))

(define (iota n)
  (unless (and (integer? n) (<= 0 n)) (error "not a nonnegative integer" n))
  (let loop ((i 0))
    (cond ((= i n) '())
          (else    (cons i (loop (+ i 1)))))))

(define range-for-each
  (let ((go (lambda (f start end inc)
              (let ((? (if (< inc 0) > <)))
                (let loop ((i start))
                  (when (? i end)
                    (f i)
                    (loop (+ i inc))))))))
    (case-lambda
      ((f end)           (go f 0 end 1))
      ((f start end)     (go f start end 1))
      ((f start end inc) (go f start end inc)))))

(define range-map
  (let ((go (lambda (f start end inc)
              (let ((? (if (< inc 0) > <)))
                (let loop ((i start))
                  (if (? i end)
                      (cons (f i) (loop (+ i inc)))
                      '()))))))
    (case-lambda
      ((f end)           (go f 0 end 1))
      ((f start end)     (go f start end 1))
      ((f start end inc) (go f start end inc)))))

(define (range . arg*) (apply range-map (lambda (x) x) arg*))

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
                                                  (loop (car x*) (cdr x*) (map1 cdr x**)))))))))))

  (define (foldl f acc x*)
    (let loop ((x* x*) (acc acc))
      (cond ((null? x*) acc)
            (else       (loop (cdr x*) (f (car x*) acc))))))

  (define (foldr f acc x*)
    (let loop ((x* x*))
      (cond ((null? x*) acc)
            (else       (f (car x*) (loop (cdr x*)))))))

  (define fold-left
    (case-lambda
      ((f acc x*) (let loop ((x* x*) (acc acc))
                    (cond ((null? x*) acc)
                          (else       (loop (cdr x*) (f acc (car x*)))))))
      ((f acc y* . y**)
       (let loop ((x* y*) (x** y**) (acc acc))
         (cond ((null? x*) (unless (andmap1 null? x**)
                             (error "lists of different length"
                                    (cons (length y*) (map1 length y**))))
                           acc)
               (else       (loop (cdr x*) (map1 cdr x**)
                                 (apply f acc (car x*) (map1 car x**)))))))))

  (define fold-right
    (case-lambda
      ((f acc x*) (let loop ((x* x*))
                    (cond ((null? x*) acc)
                          (else       (f (loop (cdr x*)) (car x*))))))
      ((f acc y* . y**)
       (let loop ((x* y*) (x** y**))
         (cond ((null? x*) (unless (andmap1 null? x**)
                             (error "lists of different length"
                                    (cons (length y*) (map1 length y**))))
                           acc)
               (else (apply f (loop (cdr x*) (map1 cdr x**)) (car x*) (map1 car x**))))))))
  )
