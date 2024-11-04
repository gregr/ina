(define (make-program)
  (mlet ((D ($d:begin)))
    (define (valid?!) (unless D (error "cannot use already-evaluated program")))
    (lambda (method)
      (case method
        ((add!)    (lambda (D.new) (valid?!) (set! D ($d:begin D D.new))))
        ((compile) (lambda (publish-defined-values?)
                     (valid?!)
                     (let ((E ((if publish-defined-values? D->E/publish D->E) D)))
                       (set! D #f)
                       E)))))))

(define (program->E         p) ((p 'compile) #f))
(define (program->E/publish p) ((p 'compile) #t))

(define (program-parse-definition*/env.d p env.d env stx*.def)
  ((p 'add!) (parse-begin-definition* env.d (env-conjoin env.d env) stx*.def)))

(define (program-parse-definition* p env stx*.def)
  (let ((env.d (make-env)))
    (program-parse-definition*/env.d p env.d env stx*.def)
    (env-read-only env.d)))

(define (eval-definition* env stx*.def)
  (let* ((p     (make-program))
         (env.d (program-parse-definition* p env stx*.def)))
    (E-eval (program->E/publish p))
    env.d))
