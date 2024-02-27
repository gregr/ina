(define (dynamic-env-ref key default-value)
  (let loop ((frame* (dynamic-env)))
    (cond ((pair?    frame*) (let ((frame (car frame*)))
                               (if (eq? (car frame) key)
                                   (cdr frame)
                                   (loop (cdr frame*)))))
          ((mvector? frame*) (loop (mvector-ref frame* 0)))  ; follow dynamic parent link
          (else              default-value))))

(define (dynamic-env-set! key value)
  (dynamic-env
    (cons (cons key value)
          (let* ((frame* (dynamic-env))
                 (depth  (let loop ((frame* frame*) (depth 0))  ; find any existing frame for key
                           (and (pair? frame*)
                                (let ((frame (car frame*)))
                                  (if (eq? (car frame) key)
                                      depth
                                      (loop (cdr frame*) (+ depth 1))))))))
            (if depth
                (let loop ((frame* frame*) (depth depth))  ; remove the existing frame
                  (if (< 0 depth)
                      (cons (car frame*) (loop (cdr frame*) (- depth 1)))
                      (cdr frame*)))
                frame*)))))

(define (make-dynamic-parameter default-value)
  (define param
    (case-lambda
      (()  (dynamic-env-ref  param default-value))
      ((x) (dynamic-env-set! param x))))
  param)

(define (with-dynamic-binding key value thunk)
  (let ((saved-denv (dynamic-env)))
    (dynamic-env-set! key value)
    (let-values ((result* (thunk)))
      (dynamic-env saved-denv)
      (apply values result*))))

(define (with-dynamic-binding* kv* thunk)
  (let ((saved-denv (dynamic-env)))
    (for-each (lambda (kv) (apply dynamic-env-set! kv)) kv*)
    (let-values ((result* (thunk)))
      (dynamic-env saved-denv)
      (apply values result*))))
