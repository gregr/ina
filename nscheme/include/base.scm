;;;;;;;;;;;;;;
;;; Errors ;;;
;;;;;;;;;;;;;;

(define (make-error    kind details) (svector 'error kind details))
(define (error-kind    err)          (error?! err) (svector-ref err 1))
(define (error-details err)          (error?! err) (svector-ref err 2))
(define (error?        x)            (and (svector? x)
                                          (= (svector-length x) 3)
                                          (eq? (svector-ref x 0) 'error)))
(define (error?!       x)            (has-type?! error? 'error? x))

(define (raise-type-error expected value)
  (raise (make-error 'type `(expected ,expected actual ,value))))

(define (has-type?! type? expected value)
  (unless (type? value) (raise-type-error expected value)))

(define (raise-syntax-error expr description)
  (raise (make-error 'syntax `(expression ,expr description ,description))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mutable vector and bytevector transformation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: define analogous operators for mbytevector

(define (mvector-transform-range! mv start end f)
  (unless (and (<= 0 start) (<= start end) (<= end (mvector-length mv)))
    (error "invalid mvector range"
           'length (mvector-length mv) 'start start 'end end))
  (let loop ((i start))
    (when (< i end)
      (mvector-set! mv i (f i))
      (loop (+ i 1)))))

(define (mvector-fill! mv v)
  (mvector-transform-range! mv 0 (mvector-length mv) (lambda (_) v)))

(define (mvector-copy! mv start src start.src end.src)
  (let-values (((ref length)
                (cond ((mvector?     src) (values mvector-ref     mvector-length))
                      ((vector?      src) (values vector-ref      vector-length))
                      ((mbytevector? src) (values mbytevector-ref mbytevector-length))
                      ((bytevector?  src) (values bytevector-ref  bytevector-length))
                      (else (error "invalid source for mvector-copy!" src)))))
    (unless (and (<= 0 start.src) (<= start.src end.src) (<= end.src (length src)))
      (error "invalid source range" 'length (length src) 'start start.src 'end end.src))
    (mvector-transform-range! mv start (+ start (- end.src start.src))
                              (lambda (i) (ref src (+ start.src (- i start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Association and property lists ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: no optional parameters
(define (alist-ref alist key (default (lambda () (error "alist-ref missing key" alist key))))
  (match (assoc key alist)
    ((cons _ v) v)
    (#f         (if (procedure? default) (default) default))))

(define (alist-update alist key v->v (default (lambda () (error "alist-ref missing key" alist key))))
  (let loop ((kvs alist) (prev '()))
    (cond ((null?        kvs     ) (define v (if (procedure? default) (default) default))
                                   (cons (cons key (v->v v)) alist))
          ((equal? (caar kvs) key) (foldl cons (cons (cons key (v->v (cdar kvs))) (cdr kvs)) prev))
          (else                    (loop (cdr kvs) (cons (car kvs) prev))))))

(define (alist-set alist key value) (alist-update alist key (lambda (_) value) #f))

(define (alist-remove alist key)
  ;; TODO: stop after the first matching key is found
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (plist->alist kvs) (if (null? kvs) '()
                             (cons (cons (car kvs) (cadr kvs))
                                   (plist->alist (cddr kvs)))))

;;;;;;;;;;;;;;;;
;;; Equality ;;;
;;;;;;;;;;;;;;;;

(define (string?!     x) (has-type?! string?     'string?     x))
(define (vector?!     x) (has-type?! vector?     'vector?     x))
(define (bytevector?! x) (has-type?! bytevector? 'bytevector? x))

(define (vector=? a b)
  (vector?! a) (vector?! b)
  (and (= (vector-length a) (vector-length b))
       (let ((end (vector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (equal? (vector-ref a i) (vector-ref b i))
                    (loop (+ i 1))))))))

(define (bytevector=? a b)
  (bytevector?! a) (bytevector?! b)
  (and (= (bytevector-length a) (bytevector-length b))
       (let ((end (bytevector-length a)))
         (let loop ((i 0))
           (or (= i end)
               (and (equal? (bytevector-ref a i) (bytevector-ref b i))
                    (loop (+ i 1))))))))

(define (string=? a b)
  (string?! a) (string?! b)
  (bytevector=? (string->utf8 a) (string->utf8 b)))

(define (equal? a b)
  (or (eqv? a b)
      (cond ((pair?       a) (and (pair? b)
                                  (equal? (car a) (car b))
                                  (equal? (cdr a) (cdr b))))
            ((vector?     a) (and (vector?     b) (vector=?     a b)))
            ((bytevector? a) (and (bytevector? b) (bytevector=? a b)))
            ((string?     a) (and (string?     b) (string=? a b)))
            (else            #f))))

;;;;;;;;;;;;;
;;; Lists ;;;
;;;;;;;;;;;;;

(splicing-local
  ((define (append2 xs y)
     (cond ((null? xs) y)
           (else       (cons (car xs) (append2 (cdr xs) y))))))
  (define append
    (case-lambda
      (()          '())
      ((xs . rest) (let loop ((xs xs) (rest rest))
                     (cond ((null? rest) xs)
                           (else         (append2 xs (loop (car rest) (cdr rest))))))))))

(define (improper-list->list xs)
  (cond ((null? xs) '())
        ((pair? xs) (cons (car xs) (improper-list->list (cdr xs))))
        (else       (list xs))))

(define (improper-list-map f xs)
  (let loop ((xs xs))
    (cond ((null? xs) '())
          ((pair? xs) (cons (f (car xs)) (loop (cdr xs))))
          (else       (f xs)))))
