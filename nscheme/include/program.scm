(define (make-program)
  (mlet ((D ($d:begin)))
    (define (valid?!) (unless D (error "cannot use already-evaluated program")))
    (lambda (method)
      (case method
        ((add!)        (lambda (D.new) (valid?!) (set! D ($d:begin D D.new))))
        ((E)           (valid?!) (let ((E (D->E D))) (set! D #f) E))
        ((evaluated-E) (valid?!) (let ((E (D->E/E-eval D E-eval))) (set! D #f) E))))))

(define (program->E           p) (p 'E))
(define (program->evaluated-E p) (p 'evaluated-E))
(define (program-eval!        p) (program->evaluated-E p) (void))
(define (program-eval         p) (E-eval (program->evaluated-E p)))

(define (program-link-definition*/env.d p env.d env stx*.def)
  ((p 'add!) (parse-begin-definition* env.d (env-conjoin env.d env) stx*.def)))

(define (program-link-definition* p env stx*.def)
  (let ((env.d (make-env)))
    (program-link-definition*/env.d p env.d env stx*.def)
    (env-freeze env.d)))

(define (eval-definition* env stx*.def)
  (let* ((p     (make-program))
         (env.d (program-link-definition* p env stx*.def)))
    (program-eval! p)
    env.d))
