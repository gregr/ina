(load "eval-fo.scm")

(define (k-value expr k) `(k-value ,expr ,k))
(define (k-proc k) `(k-proc ,k))
(define (k-args-clear k) `(k-args-clear ,k))
(define (k-args-push k) `(k-args-push ,k))
(define k-apply '(k-apply))
(define k-return-pop '(k-return-pop))
(define (k-return-push k return-k) `(k-return-push ,k ,return-k))
(define (k-branch k-true k-false) `(k-branch ,k-true ,k-false))
(define (k-join k) `(k-join ,k))
(define k-halt '(k-halt))

(define (direct->k k expr)
  (define (direct->k-arg earg k) (direct->k (k-args-push k) earg))
  (case (car expr)
    ((literal reference) (k-value expr k))
    ((lambda)
     (k-value `(lambda ,(cadr expr) ,(direct->k k-return-pop (caddr expr))) k))
    ((if) (direct->k (k-branch (direct->k (k-join k) (caddr expr))
                               (direct->k (k-join k) (cadddr expr)))
                     (cadr expr)))
    ((application)
     (define app-k
       (k-args-clear (k-proc (list-foldl direct->k-arg k-apply (caddr expr)))))
     (direct->k (if (eq? 'k-return-pop (car k)) app-k (k-return-push app-k k))
                (cadr expr)))
    (else (error 'direct->k (format "invalid expression ~s" expr)))))

(define (apply-primitive-k pname args)
  (case pname
    ((cons) (apply cons args))
    ((car) (apply car args))
    ((cdr) (apply cdr args))
    ((=) (apply = args))
    ((boolean=?) (apply boolean=? args))
    ((symbol=?) (apply symbol=? args))
    ((null?) (apply null? args))
    ((pair?) (apply pair? args))
    ((symbol?) (apply symbol? args))
    ((number?) (apply number? args))
    ((procedure?)
     (if (= 1 (length args))
       (procedure-fo? (car args))
       (error 'apply-procedure-fo?
              (format "procedure? expects 1 argument ~s" args))))
    ((apply)
     (apply (lambda (proc args)
              (apply-k proc args (list (list k-halt #f #f '())))) args))
    ((vector) (vector-fo (apply vector args)))
    ((vector?) (apply vector-fo? args))
    ((vector-length) (apply vector-fo-length args))
    ((vector-ref) (apply vector-fo-ref args))
    (else (error 'apply-primitive (format "invalid primitive ~s" pname)))))

(define (apply-k proc args returns)
  (define (err) (error 'apply-k (format "invalid procedure ~s" proc)))
  (if (procedure-fo? proc)
    (let ((proc (tagged-payload proc)))
      (cond
        ((symbol? proc)
         (evaluate-k
           k-return-pop (apply-primitive-k proc args) #f #f #f returns))
        ((and (pair? proc) (eq? 'closure (car proc)))
         (define env (env-extend* (cadr proc) (caddr proc) args))
         (evaluate-k (cadddr proc) #f #f '() env returns))
        (else (err))))
    (err)))

(define (evaluate-k k result proc args env returns)
  (case (car k)
    ((k-value)
     (define expr (cadr k))
     (define result
       (case (car expr)
         ((literal) (cadr expr))
         ((reference) (env-ref env (cadr expr)))
         ((lambda) (procedure-fo `(closure ,env ,(cadr expr) ,(caddr expr))))))
     (evaluate-k (caddr k) result proc args env returns))
    ((k-return-push)
     (evaluate-k (cadr k) result proc args env
                 (cons (list (caddr k) proc args env) returns)))
    ((k-proc) (evaluate-k (cadr k) result result args env returns))
    ((k-args-clear) (evaluate-k (cadr k) result proc '() env returns))
    ((k-args-push)
     (evaluate-k (cadr k) result proc (cons result args) env returns))
    ((k-apply) (apply-k proc args returns))
    ((k-return-pop)
     (define r (car returns))
     (evaluate-k (car r) result (cadr r) (caddr r) (cadddr r) (cdr returns)))
    ((k-branch)
     (evaluate-k (if result (cadr k) (caddr k)) result proc args env returns))
    ((k-join) (evaluate-k (cadr k) result proc args env returns))
    ((k-halt) result)
    (else (error 'evaluate-k (format "invalid expression ~s" expr)))))

(define (evaluate expr env)
  (evaluate-k (direct->k k-halt (denote expr env)) #f #f '() env '()))
