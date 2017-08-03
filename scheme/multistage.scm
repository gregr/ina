(load "common.scm")

(define (ms-denote* expr* env level)
  (map (lambda (e) (ms-denote e env level)) expr*))

(define (ms-denote expr env level)
  (when (< level 0) (error 'ms-denote (format "invalid level for ~s" expr)))
  (cond
    ((or (boolean? expr) (number? expr)) expr)
    ((symbol? expr) (env-ref env (env-address env expr)))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (let loop ((expr expr))
           (case-pattern
             expr  ;; #,@ is only valid for procedure application code.
             `((((unsyntax-splicing #f))
                ,(lambda (rest)
                   (list (if (= 1 level) 'unquote 'unsyntax)
                         (ms-denote rest env (- level 1)))))
               ((#f) ,(lambda (e) `(,(ms-denote e env level))))
               ((#f . #f) ,(lambda (ea ed)
                             `(,(ms-denote ea env level) . ,(loop ed))))
               (#f ,(lambda _ (error 'ms-denote ("invalid application syntax ~s"
                                                 expr)))))))
         (case head
           ((quote) `(quote ,(car (syntax-pattern '(quote #f) expr))))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts))
                   (ms-params (let loop ((params params))
                                (cond
                                  ((null? params) '())
                                  ((symbol? params)
                                   (gensym (symbol->string params)))
                                  ((pair? params)
                                   (cons (gensym (symbol->string (car params)))
                                         (loop (cdr params))))))))
              `(lambda ,ms-params
                 ,(ms-denote body (env-extend* env params ms-params) level))))
           ((quasisyntax)
            (let ((expr (car (syntax-pattern '(quasisyntax #f) expr))))
              (list (if (= 0 level) 'quasiquote 'quasisyntax)
                    (ms-denote expr env (+ level 1)))))
           ((unsyntax)
            (let ((expr (car (syntax-pattern '(unsyntax #f) expr))))
              (list (if (= 1 level) 'unquote 'unsyntax)
                    (ms-denote expr env (- level 1)))))
           ((if) (let* ((parts (syntax-pattern '(if #f #f #f) expr)))
                   `(if ,(ms-denote (car parts) env level)
                      ,(ms-denote (cadr parts) env level)
                      ,(ms-denote (caddr parts) env level))))
           ;; TODO: quasiquote
           (else (error 'ms-denote (format "unbound variable ~s" head)))))))
    (else (error 'ms-denote (format "invalid syntax ~s" expr)))))

(define (ms-eval expr env) (eval (ms-denote expr env 0)))

(define (test-denote* expr* env) (map (lambda (e) (test-denote e env)) expr*))

(define (test-lambda-args params args)
  (cond
    ((null? params) '())
    ((symbol? params) args)
    ((pair? params)
     (cons `(car ,args) (test-lambda-args (cdr params) `(cdr ,args))))))

;; This extends the language with let[*] without real interpretive overhead.
(define (test-denote expr env)
  (define (test-lambda params body env)
    (ms-eval
      '#`(lambda args
           #,(test-denote
               body (env-extend*
                      env params (test-lambda-args params #`args))))
      (env-extend-bindings
        env-initial
        `((body . (quote ,body))
          (env . (quote ,env))
          (params . (quote ,params))))))
  (cond
    ((or (boolean? expr) (number? expr)) expr)
    ((symbol? expr) (env-ref env (env-address env expr)))
    ((pair? expr)
     (let ((head (car expr)))
       (if (or (not (symbol? head)) (bound? env head))
         (ms-eval '#`(#,(test-denote head env) #,@(test-denote* (cdr expr) env))
                  (env-extend-bindings
                    env-initial
                    `((head . (quote ,head))
                      (env . (quote ,env))
                      (expr . (quote ,expr)))))
         (case head
           ((quote)
            (let ((datum (car (syntax-pattern '(quote #f) expr))))
              (ms-eval '#`datum (env-extend-bindings
                                  env-initial `((datum . (quote ,datum)))))))
           ((lambda)
            (let* ((parts (syntax-pattern '(lambda #f #f) expr))
                   (params (car parts))
                   (body (cadr parts)))
              (test-lambda params body env)))

           ((let)
            (let* ((parts (syntax-pattern '(let #f #f) expr))
                   (bindings (map (lambda (b) (syntax-pattern '(#f #f) b))
                                  (car parts)))
                   (body (cadr parts))
                   (params (map car bindings))
                   (args (map cadr bindings)))
              (ms-eval '#`(#,(test-lambda params body env)
                           #,@(test-denote* args env))
                       (env-extend-bindings
                         env-initial
                         `((test-lambda . (quote ,test-lambda))
                           (body . (quote ,body))
                           (env . (quote ,env))
                           (params . (quote ,params))
                           (args . (quote ,args)))))))
           ((let*)
            (let* ((parts (syntax-pattern '(let* #f #f) expr))
                   (bindings (map (lambda (b) (syntax-pattern '(#f #f) b))
                                  (car parts)))
                   (body (cadr parts))
                   (params (map car bindings))
                   (args (map cadr bindings)))
              (let loop ((params params) (args args) (env env))
                (if (null? params)
                  (test-denote body env)
                  (ms-eval
                    '#`((lambda (arg)
                          #,(loop (cdr params) (cdr args)
                                  (env-extend*
                                    env (list (car params)) (list #`arg))))
                        #,(test-denote (car args) env))
                    (env-extend-bindings
                      env-initial
                      `((loop . (quote ,loop))
                        (env . (quote ,env))
                        (params . (quote ,params))
                        (args . (quote ,args)))))))))
           ;; TODO: if, quasiquote
           (else (error 'test-denote (format "unbound variable ~s" head)))))))))

(define env-initial
  (env-extend-bindings
    env-empty
    `((cons . cons)
      (car . car)
      (cdr . cdr)
      (not . not)
      (equal? . equal?)
      (pair? . pair?)
      (symbol? . symbol?)
      (number? . number?)
      (procedure? . procedure?)
      (vector? . vector?)
      (vector . vector)
      (vector-length . vector-length)
      (vector-ref . vector-ref)
      (apply . apply)
      (list . list)
      (+ . +)
      (test-denote . test-denote)
      (test-denote* . test-denote*)
      (test-lambda-args . test-lambda-args)
      (env-extend* . env-extend*)
      )))

(define test0a-failure
  ((lambda (eta)
     `(lambda (x)
        ,(eta (lambda (y)
                `(+ x ,y)))))
   (lambda (f)
     `(lambda (x)
        ,(f `x)))))

(define test0
  (ms-eval
    '((lambda (eta)
        #`(lambda (x)
            #,(eta (lambda (y)
                     #`(+ x #,y)))))
      (lambda (f)
        #`(lambda (x)
            #,(f #`x))))
    env-initial))

(define test0a
  (ms-eval
    '#`((lambda (eta)
          #`(lambda (x)
              #,(eta (lambda (y)
                       #`(+ x #,y)))))
        (lambda (f)
          #`(lambda (x)
              #,(f #`x))))
    env-initial))

(define test0b
  (ms-eval
    '#`(lambda (x)
         #,((lambda (t f)
              #`(if (cdr '(#t . #f)) #,t #,f)) #`x 'no))
    env-initial))

(define test0c
  (ms-eval
    '#`(lambda (x)
         #,((lambda (t f)
              (if (cdr '(#t . #f)) t f)) #`x 'no))
    env-initial))

(define test0d
  (ms-eval
    '#`(lambda (x)
         ((lambda (t f)
            #,(if (cdr '(#t . #f)) #`t #`f)) x 'no))
    env-initial))

(define test1
  (ms-eval
    '#`((lambda args (list args args)) #,@(list 1 2))
    env-initial))

(define test2
  (test-denote
    '(lambda (q) q)
    env-initial))

(define test3
  (test-denote
    '((lambda (q) (list 'q q)) 5)
    env-initial))

(define test4
  (test-denote
    '(let ((x (+ 2 1)) (y (+ 2 2))) (list y x))
    env-initial))

(define test5
  (test-denote
    '(let* ((x (+ 2 1)) (y (+ 2 2)) (x (list 'x y)) (y (list 'y 5)))
       (list y x))
    env-initial))
