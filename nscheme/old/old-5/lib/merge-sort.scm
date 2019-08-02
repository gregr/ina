((provide merge-sort))

(define (list-odds xs)
  (cond ((null? xs)       '())
        ((null? (cdr xs)) xs)
        (#t               (cons (car xs) (list-odds (cddr xs))))))

(define (merge-sort merge xs)
  (cond ((null? xs)       '())
        ((null? (cdr xs)) xs)
        (#t               (merge (merge-sort merge (list-odds xs))
                                 (merge-sort merge (list-odds (cdr xs)))))))
