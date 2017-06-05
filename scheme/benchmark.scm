(define problem-iterations 100)
(define problem-size 100)

(define (ev expr) (evaluate expr env-initial))

(define-syntax bench
  (syntax-rules ()
    ((_ name expr)
     (begin
       (printf "Benchmarking ~s\n" name)
       (let ((result (time expr))) (display ""))))))

(bench 'append
  (ev `(let ((fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a)))))))
             (null? (lambda (x) (equal? '() x)))
             (list (lambda xs xs)))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           (list . ,(make-list problem-iterations
                               `(append ',(make-list problem-size 'x) '())))))))

(bench 'reverse-quadratic
  (ev `(let ((fix (lambda (f)
                    ((lambda (d) (d d))
                     (lambda (x) (f (lambda a (apply (x x) a)))))))
             (null? (lambda (x) (equal? '() x)))
             (list (lambda xs xs)))
         (let ((append
                 (fix (lambda (append)
                        (lambda (xs ys)
                          (if (null? xs)
                            ys
                            (cons (car xs) (append (cdr xs) ys))))))))
           (let ((reverse
                   (fix (lambda (reverse)
                          (lambda (xs)
                            (if (null? xs)
                              '()
                              (append (reverse (cdr xs)) (list (car xs)))))))))
             (list . ,(make-list problem-iterations
                               `(reverse ',(make-list problem-size 'x)))))))))
