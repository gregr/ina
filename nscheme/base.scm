(define (append2 xs ys) (if (null? ys) xs (foldr cons ys xs)))
(define (append . xss)  (foldr append2 '() xss))

(define (reverse-append xs ys) (foldl cons ys xs))
(define (string->list s) (vector->list (string->vector s)))
(define (list->string cs) (vector->string (list->vector cs)))
(define (rlist->string cs) (list->string (reverse cs)))
(define (mvector->list mv) (vector->list (mvector->vector mv)))
(define (mvector->string mv) (vector->string (mvector->vector mv)))
;; TODO: this operation should be removed
(define (string-length s) (vector-length (string->vector s)))
;; TODO: this operation should be removed
(define (string-ref s i) (vector-ref (string->vector s) i))

(define (mvector-fill! mv v)
  (let loop ((i (- (mvector-length mv) 1)))
    (when (<= 0 i) (mvector-set! mv i v) (loop (- i 1)))))
(define (mvector-copy!/ref mv start src src-start src-end ref)
  (let loop ((i src-start))
    (cond ((<= src-end i) (- i src-start))
          (else (mvector-set! mv (+ start (- i src-start)) (ref src i))
                (loop (+ i 1))))))
(define (mvector-copy! mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end mvector-ref))
(define (mvector-copy!/vector mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end vector-ref))
;; TODO: this operation needs to be removed
(define (mvector-copy!/string mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end string-ref))
(define (mvector-copy!/list mv start xs)
  (let loop ((i start) (xs xs)) (cond ((null? xs) (- i start))
                                      (else       (mvector-set! mv i (car xs))
                                                  (loop (+ i 1) (cdr xs))))))

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

(define-syntax-rule (let/cps name params expr body ...)
  (let ((name (lambda params body ...))) expr))
