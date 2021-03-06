((provide tagged-vector? tagged-vector?! case/import)
 (require test))

(define (import-apply i env)
  (apply (cdr i) (map (lambda (name)
                        (cdr (or (assoc name env)
                                 (error '"missing argument:" name))))
                      (car i))))

(define (tagged-vector? tag field-names d)
  (define len (+ 1 (length field-names)))
  (and (vector? d) (= len (vector-length d))
       (equal? tag (vector-ref d 0))
       (map (lambda (name i) (cons name (vector-ref d i)))
            field-names (cdr (range len)))))

(define (tagged-vector?! tag fnames d)
  (or (tagged-vector? tag fnames d)
      (error '"invalid tagged vector:" (cons tag fnames) d)))

(define (case/import d clause*)
  ((or (ormap (lambda (clause)
                (let ((env ((car clause) d)))
                  (and env (lambda () (import-apply (cdr clause) env)))))
              clause*)
       (error '"no matching case/import clause:" d))))

(when test
  (define example:tag         'example)
  (define (example x y)       (vector example:tag x y))
  (define (example? d)        (tagged-vector? example:tag '(x y) d))
  (define (example?! d)       (tagged-vector?! example:tag '(x y) d))
  (define (example-x e)       (import-apply (cons '(x) (lambda (x) x))
                                            (example?! e)))
  (define (example-y e)       (import-apply (cons '(y) (lambda (y) y))
                                            (example?! e)))
  (define (example-x-set e x) (example?! e) (example x (example-y e)))

  (define example2:tag        'example2)
  (define (example2 z)        (vector example2:tag z))
  (define (example2? d)       (tagged-vector? example2:tag '(z) d))

  (test 'example-1
    (example? (example 1 2))
    '((x . 1) (y . 2)))
  (test 'example-2
    (example? (example2 3))
    #f)
  (test 'example-3
    (example? #t)
    #f)
  (test 'example-4
    (example-x (example 2 3))
    2)
  (test 'example-5
    (example-x (example-x-set (example 2 3) 5))
    5)

  (test 'case/import-1
    (case/import
      (example 1 2)
      (list (cons example?       (cons '(x y) (lambda (x y)
                                                (list 'first x y))))
            (cons example2?      (cons '(z)   (lambda (z) (list 'second z))))
            (cons (lambda _ '()) (cons '()    (lambda () 'third)))))
    '(first 1 2))
  (test 'case/import-2
    (case/import
      (example2 3)
      (list (cons example?       (cons '(x y) (lambda (x y)
                                                (list 'first x y))))
            (cons example2?      (cons '(z)   (lambda (z) (list 'second z))))
            (cons (lambda _ '()) (cons '()    (lambda () 'third)))))
    '(second 3))
  (test 'case/import-3
    (case/import
      55
      (list (cons example?       (cons '(x y) (lambda (x y)
                                                (list 'first x y))))
            (cons example2?      (cons '(z)   (lambda (z) (list 'second z))))
            (cons (lambda _ '()) (cons '()    (lambda () 'third)))))
    'third)

  )
