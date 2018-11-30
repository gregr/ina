(provide base:stage base:ast:values)

(require stage env:primitive language
         primitive-op-descriptions primitive-op-type-signature)

(define primitive-op-procs
  (map (lambda (po-desc)
         (define (x i) (vector-ref '#(x0 x1 x2 x3 x4) i))
         (define type-sig (primitive-op-type-signature po-desc))
         (define p* (map x (range (length (car type-sig)))))
         (list (car po-desc) (list 'lambda p* (cons (car po-desc) p*))))
       primitive-op-descriptions))

(define derived-op-procs
  '((error (lambda args ('error args)))
    (not (lambda (b) (if b #f #t)))
    (caar (lambda (v) (car (car v))))
    (cadr  (lambda (xs) (car (cdr xs))))
    (cdar (lambda (v) (cdr (car v))))
    (cadar (lambda (v) (cadr (car v))))
    (caddr (lambda (xs) (cadr (cdr xs))))
    (list-tail (lambda (xs i) (if (= 0 i) xs (list-tail (cdr xs) (- i 1)))))
    (list-ref  (lambda (xs i) (car (list-tail xs i))))
    (list->vector (lambda (xs)
                    (define result (make-mvector (length xs) #t))
                    (foldl (lambda (x i) (mvector-set! result i x) (+ i 1))
                           0 xs)
                    (mvector->vector result)))
    (vector->list (lambda (v)
                    (let loop ((i (- (vector-length v) 1)) (xs '()))
                      (if (< i 0) xs
                        (loop (- i 1) (cons (vector-ref v i) xs))))))
    (equal? (lambda (a b)
              (cond ((pair? a)   (and (pair? b) (equal? (car a) (car b))
                                      (equal? (cdr a) (cdr b))))
                    ((vector? a) (and (vector? b) (equal? (vector->list a)
                                                          (vector->list b))))
                    ((boolean? a)   (and (boolean? b)   (boolean=? a b)))
                    ((string?  a)   (and (string?  b)   (string=?  a b)))
                    ((number?  a)   (and (number?  b)   (number=?  a b)))
                    ((mvector? a)   (and (mvector? b)   (mvector=? a b)))
                    ((procedure? a) (and (procedure? b) (procedure=? a b)))
                    ((null? a)      (null? b)))))
    (vector (lambda xs (list->vector xs)))
    (list?  (lambda (v) (or (and (pair? v) (list? (cdr v))) (null? v))))
    (list   (lambda xs xs))
    (list*  (lambda (x . xs) (if (null? xs) x (cons x (apply list* xs)))))
    ;; TODO: n-ary versions of foldl, foldr.
    (foldl  (lambda (f acc xs) (if (null? xs) acc
                                 (foldl f (f (car xs) acc) (cdr xs)))))
    (foldr  (lambda (f acc xs) (if (null? xs) acc
                                 (f (car xs) (foldr f acc (cdr xs))))))
    (map (lambda (f xs . xss)
           (define (map1 f xs) (if (null? xs) '()
                                 (cons (f (car xs)) (map1 f (cdr xs)))))
           (cond ((null? xs) '())
                 (#t (cons (apply f (car xs) (map1 car xss))
                           (apply map f (cdr xs) (map1 cdr xss)))))))
    (for-each (lambda args (apply map args) #t))
    (andmap (lambda (f xs . xss)
              (let loop ((last #t) (xs xs) (xss xss))
                (and last (if (null? xs) last
                            (loop (apply f (car xs) (map car xss))
                                  (cdr xs) (map cdr xss)))))))
    (ormap (lambda (f xs . xss)
             (cond ((null? xs) #f)
                   ((apply f (car xs) (map car xss)))
                   (#t (apply ormap f (cdr xs) (map cdr xss))))))
    (filter (lambda (p? xs)
              (cond ((null? xs) '())
                    ((p? (car xs)) (cons (car xs) (filter p? (cdr xs))))
                    (#t (filter p? (cdr xs))))))
    (filter-not (lambda (p? xs) (filter (lambda (x) (not (p? x))) xs)))
    (remf (lambda (p? xs)
            (cond ((null? xs)    '())
                  ((p? (car xs)) (cdr xs))
                  (#t (cons (car xs) (remf p? (cdr xs)))))))
    (remove (lambda (v xs) (remf (lambda (x) (equal? x v)) xs)))
    (length (lambda (xs) (foldl (lambda (_ l) (+ 1 l)) 0 xs)))
    (append (lambda xss (foldr (lambda (xs yss) (foldr cons yss xs)) '() xss)))
    (reverse-append (lambda (xs ys) (foldl cons ys xs)))
    (reverse (lambda (xs) (reverse-append xs '())))
    (range (lambda (n)
             (let loop ((i 0)) (if (= i n) '() (cons i (loop (+ i 1)))))))
    (take (lambda (xs n) (if (= 0 n) '()
                           (cons (car xs) (take (cdr xs) (- n 1))))))
    (drop (lambda (xs n) (if (= 0 n) xs (drop (cdr xs) (- n 1)))))
    (memf (lambda (? xs) (cond ((null? xs) #f)
                               ((? (car xs)) xs)
                               (#t (memf ? (cdr xs))))))
    (member (lambda (v xs) (memf (lambda (x) (equal? x v)) xs)))
    (assoc (lambda (k xs) (cond ((null? xs) #f)
                                ((equal? k (caar xs)) (car xs))
                                (#t (assoc k (cdr xs))))))
    (alist-get (lambda (rs key default) (let ((rib (assoc key rs)))
                                          (if rib (cdr rib) default))))
    (alist-remove* (lambda (rs keys)
                     (filter (lambda (rib) (not (member (car rib) keys))) rs)))
    (string-append (lambda ss
                     (define css (map vector->list (map string->vector ss)))
                     (vector->string (list->vector (apply append css)))))
    ))

(def #(base:stage base:ast:values)
     (language stage env:primitive #f
               (list (cons 'let primitive-op-procs)
                     '(let (apply (lambda (f arg . args)
                                    (define (cons* x xs)
                                      (if (null? xs) x
                                        (cons x (cons* (car xs) (cdr xs)))))
                                    (apply f (cons* arg args)))))
                     (cons 'letrec derived-op-procs))
               (lambda _ '())))
