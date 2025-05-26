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

(define (program-parse-definition*/env p env stx*.def)
  ((p 'add!) (parse-begin-definition* env stx*.def)))

(define (program-parse-definition* p env stx*.def)
  (let* ((env.d (make-env))
         (env   (env-read-and-write (env-conjoin env.d env) env.d)))
    (program-parse-definition*/env p env stx*.def)
    (env-freeze! env.d)
    env.d))

(define ((eval-definition*/yield yield) env stx*.def)
  (let* ((p     (make-program))
         (env.d (program-parse-definition* p env stx*.def)))
    (apply/values yield (E-eval (program->E/publish p)))
    env.d))

(define eval-definition* (eval-definition*/yield void))
