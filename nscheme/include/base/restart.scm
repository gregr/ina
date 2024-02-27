(define (make-restart name desc effector) (vector name desc effector))
(define (restart-name        r) (vector-ref r 0))
(define (restart-description r) (vector-ref r 1))
(define (restart-effector    r) (vector-ref r 2))

(define current-restart* (make-dynamic-parameter '()))
(define (find-restart name)
  (let ((r* (memp (lambda (r) (equal? (restart-name r) name)) (current-restart*))))
    (and r* (car r*))))
(define (invoke-restart name . arg*)
  (let ((r (find-restart name)))
    (when r (apply (restart-effector r) arg*))))

(define (with-raw-restart name desc effector thunk)
  (with-temporary-dynamic-env
    (lambda ()
      (current-restart* (cons (make-restart name desc effector) (current-restart*)))
      (thunk))))
(define (with-raw-restart* nde* thunk)
  (with-temporary-dynamic-env
    (lambda ()
      (current-restart* (append (map (lambda (nde) (apply make-restart nde)) nde*) (current-restart*)))
      (thunk))))
(define (with-restart name desc effector thunk)
  (let ((tag (make-escape-prompt-tag)))
    (with-escape-prompt
      tag
      (lambda (effector) (effector))
      (lambda () (with-raw-restart
                   name desc
                   (lambda x* (escape-to-prompt tag (lambda () (apply effector x*))))
                   thunk)))))
(define (with-restart* nde* thunk)
  (let ((tag (make-escape-prompt-tag)))
    (with-escape-prompt
      tag
      (lambda (effector) (effector))
      (lambda ()
        (with-raw-restart*
          (map (lambda (nde)
                 (apply (lambda (name desc effector)
                          (list name desc
                                (lambda x* (escape-to-prompt tag (lambda () (apply effector x*))))))
                        nde))
               nde*)
          thunk)))))
(define (with-simple-restart name desc thunk) (with-restart name desc (lambda () (values)) thunk))

(define (with-restart:abort desc thunk) (with-simple-restart 'abort desc thunk))
(define (with-restart:continue desc thunk) (with-simple-restart 'continue desc thunk))
(define (with-restart:retry desc thunk) (let loop () (with-restart 'retry loop thunk)))
(define (with-restart:use-value desc thunk) (with-restart 'use-value desc (lambda (x) x) thunk))
(define (with-restart:use-values desc thunk) (with-restart 'use-values desc values thunk))

(define (with-restart:continue/choice* desc thunk . thunk*)
  (let loop ((thunk thunk) (thunk* thunk*))
    (if (null? thunk*)
        (thunk)
        (with-restart 'continue desc
                      (lambda () (loop (car thunk*) (cdr thunk*)))
                      thunk))))
