(define (append2 xs ys) (if (null? ys) xs (foldr cons ys xs)))
(define (append . xss)  (foldr append2 '() xss))

(define (reverse-append xs ys) (foldl cons ys xs))
(define (string->list s) (vector->list (string->vector s)))
(define (list->string cs) (vector->string (list->vector cs)))
(define (rlist->string cs) (list->string (reverse cs)))

(define (mvector-fill! mv v)
  (let loop ((i (- (mvector-length mv) 1)))
    (when (<= 0 i) (mvector-set! mv i v) (loop (- i 1)))))
(define (mvector-copy! dest dest-start src src-start src-end)
  (let loop ((di dest-start) (si src-start))
    (when (< si src-end)
      (mvector-set! dest di (mvector-ref src si))
      (loop (+ di 1) (+ si 1)))))
(define (mvector-copy!/list mv start xs)
  (let loop ((i start) (xs xs)) (unless (null? xs)
                                  (mvector-set! mv i (car xs))
                                  (loop (+ i 1) (cdr xs)))))

(define-syntax (define-tuple stx)
  (syntax-case stx ()
    ((_ (name field ...))
     (andmap identifier? (syntax->list #'(name field ...)))
     (let ((name-string (symbol->string (syntax->datum #'name)))
           (len         (length (syntax->list #'(field ...)))))
       (with-syntax
         ((dlen     (datum->syntax #'_ len))
          ((df ...) (datum->syntax
                      #'_ (map (lambda (f)
                                 (datum->syntax
                                   f (string->symbol
                                       (string-append
                                         name-string "-"
                                         (symbol->string (syntax->datum f))))))
                               (syntax->list #'(field ...)))))
          ((di ...) (datum->syntax #'_ (range 0 len))))
         #'(begin (define (name field ...) (vector field ...))
                  (define (df d) (vector-ref d di)) ...))))))
(define-syntax-rule (define-tuple* td tds ...)
  (begin (define-tuple td) (define-tuple tds) ...))
(define-syntax (define-variant stx)
  (syntax-case stx ()
    ((_ (tag field ...))
     (andmap identifier? (syntax->list #'(tag field ...)))
     (let ((tag-string (symbol->string (syntax->datum #'tag)))
           (len        (length (syntax->list #'(tag field ...)))))
       (with-syntax
         ((dlen     (datum->syntax #'_ len))
          (d?       (datum->syntax
                      #'tag (string->symbol (string-append tag-string "?"))))
          ((df ...) (datum->syntax
                      #'_ (map (lambda (f)
                                 (datum->syntax
                                   f (string->symbol
                                       (string-append
                                         tag-string "-"
                                         (symbol->string (syntax->datum f))))))
                               (syntax->list #'(field ...)))))
          ((di ...) (datum->syntax #'_ (range 1 len))))
         #'(begin (define (tag field ...) (vector 'tag field ...))
                  (define (d? d) (and (vector? d) (= dlen (vector-length d))
                                      (eq? 'tag (vector-ref d 0))))
                  (define (df d) (vector-ref d di)) ...))))))
(define-syntax-rule (define-variant* vd vds ...)
  (begin (define-variant vd) (define-variant vds) ...))
