(define (make-program)
  (mlet ((D ($d:begin)))
    (define (build D->E) (let ((E (D->E D))) (set! D ($d:expression (lambda () E))) E))
    (lambda (method)
      (case method
        ((add!)         (lambda (D.new) (set! D ($d:begin D D.new))))
        ((D->E)         (build D->E))
        ((D->E/publish) (build D->E/publish))))))

(define (program->E         p) (p 'D->E))
(define (program->E/publish p) (p 'D->E/publish))

(define (program-parse-definition*/env.d p env.d env stx*.def)
  ((p 'add!) (parse-begin-definition* env.d (env-conjoin env.d env) stx*.def)))

(define (program-parse-definition* p env stx*.def)
  (let ((env.d (make-env)))
    (program-parse-definition*/env.d p env.d env stx*.def)
    (env-read-only env.d)))

(define ((eval-definition*/yield yield) env stx*.def)
  (let* ((p     (make-program))
         (env.d (program-parse-definition* p env stx*.def)))
    (apply/values yield (E-eval (program->E/publish p)))
    env.d))

(define eval-definition* (eval-definition*/yield void))
