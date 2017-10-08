(load "common.scm")

(define (tagged tag datum) (vector tag datum))
(define (tagged? tag datum)
  (and (vector? datum)
       (= 2 (vector-length datum))
       (eqv? tag (vector-ref datum 0))))
(define (tagged-payload datum) (vector-ref datum 1))

(define procedure-tag 'procedure)
(define (procedure-fo proc) (tagged procedure-tag proc))
(define (procedure-fo? datum) (tagged? procedure-tag datum))

(define (primitive name) (procedure-fo name))
(define (primitive? datum)
  (and (procedure-fo? datum) (symbol? (tagged-payload datum))))

(define vector-tag 'vector)
(define (vector-fo vec) (tagged vector-tag vec))
(define (vector-fo? datum) (tagged? vector-tag datum))
(define (vector-fo-length v)
  (if (vector-fo? v)
    (vector-length (tagged-payload v))
    (error 'vector-fo-length
           (format "expected a vector ~s" v))))
(define (vector-fo-ref v i)
  (if (vector-fo? v)
    (vector-ref (tagged-payload v) i)
    (error 'vector-fo-ref
           (format "expected first argument to be a vector ~s" v))))
(define (vector-reify vfo)
  (if (vector-fo? vfo)
    (tagged-payload vfo)
    (error 'vector-reify (format "invalid vector ~s" vfo))))

(define (denote-reference env addr name) `(reference ,addr ,name))
(define (denote-literal value) `(literal ,value))
(define (denote-vector ds)
  `(application ,(denote-literal (primitive 'vector)) ,ds))
(define (denote-pair da dd)
  `(application ,(denote-literal (primitive 'cons)) (,da ,dd)))
(define (denote-procedure pdbody params env)
  (let ((dbody (pdbody (env-extend-param* env params))))
    `(lambda ,params ,dbody)))
(define (denote-application dproc dargs)
  `(application ,dproc ,dargs))
(define (denote-if dc tdt tdf) `(if ,dc ,(tdt) ,(tdf)))

(define (apply-primitive pname args)
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
    ((apply) (apply apply-fo args))
    ((vector) (vector-fo (apply vector args)))
    ((vector?) (apply vector-fo? args))
    ((vector-length) (apply vector-fo-length args))
    ((vector-ref) (apply vector-fo-ref args))
    (else (error 'apply-primitive (format "invalid primitive ~s" pname)))))

(define (apply-fo proc args)
  (define (err) (error 'apply-fo (format "invalid procedure ~s" proc)))
  (if (procedure-fo? proc)
    (let ((proc (tagged-payload proc)))
      (cond
        ((symbol? proc) (apply-primitive proc args))
        ((and (pair? proc) (eq? 'closure (car proc)))
         (evaluate-fo (caddr (cdr proc))
                      (env-extend* (cadr proc) (caddr proc) args)))
        (else (err))))
    (err)))

(define (evaluate-fo expr env)
  (case (car expr)
    ((literal) (cadr expr))
    ((reference) (env-ref env (cadr expr)))
    ((application)
     (apply-fo
       (evaluate-fo (cadr expr) env)
       (map (lambda (arg) (evaluate-fo arg env)) (caddr expr))))
    ((if)
     (if (evaluate-fo (cadr expr) env)
       (evaluate-fo (caddr expr) env)
       (evaluate-fo (cadddr expr) env)))
    ((lambda) (procedure-fo `(closure ,env ,(cadr expr) ,(caddr expr))))
    (else (error 'evaluate-fo (format "invalid expression ~s" expr)))))


(define (k-value expr k) `(k-value ,expr ,k))
(define (k-proc k) `(k-proc ,k))
(define (k-arg k) `(k-arg ,k))
(define k-apply '(k-apply))
(define k-return-pop '(k-return-pop))
(define (k-return-push k return-k) `(k-return-push ,k ,return-k))
(define (k-branch k-true k-false) `(k-branch ,k-true ,k-false))
(define k-halt '(k-halt))

(define (direct->k k expr)
  (define (direct->k-arg earg k) (direct->k (k-arg k) earg))
  (case (car expr)
    ((literal reference) (k-value expr k))
    ((lambda)
     (k-value `(lambda ,(cadr expr) ,(direct->k k-return-pop (caddr expr))) k))
    ((if) (direct->k (k-branch (direct->k k (caddr expr))
                               (direct->k k (cadddr expr)))
                     (cadr expr)))
    ((application)
     (define app-k (k-proc (list-foldl direct->k-arg k-apply (caddr expr))))
     (direct->k (if (eq? 'k-return-pop (car k)) app-k (k-return-push app-k k))
                (cadr expr)))
    (else (error 'direct->k (format "invalid expression ~s" expr)))))

(define (apply-k proc args returns)
  (define (err) (error 'apply-k (format "invalid procedure ~s" proc)))
  (if (procedure-fo? proc)
    (let ((proc (tagged-payload proc)))
      (cond
        ((symbol? proc)
         (evaluate-k
           k-return-pop (apply-primitive proc args) #f #f #f returns))
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
     (evaluate-k (cadr k) result #f '() env
                 (cons (list (caddr k) proc args env) returns)))
    ((k-proc) (evaluate-k (cadr k) result result args env returns))
    ((k-arg) (evaluate-k (cadr k) result proc (cons result args) env returns))
    ((k-apply) (apply-k proc args returns))
    ((k-return-pop)
     (define r (car returns))
     (evaluate-k (car r) result (cadr r) (caddr r) (cadddr r) (cdr returns)))
    ((k-branch)
     (evaluate-k (if result (cadr k) (caddr k)) result proc args env returns))
    ((k-halt) result)
    (else (error 'evaluate-k (format "invalid expression ~s" expr)))))


(define env-initial
  (env-extend-bindings
    env-empty
    `((cons . ,(primitive 'cons))
      (car . ,(primitive 'car))
      (cdr . ,(primitive 'cdr))
      (= . ,(primitive '=))
      (boolean=? . ,(primitive 'boolean=?))
      (symbol=? . ,(primitive 'symbol=?))
      (null? . ,(primitive 'null?))
      (pair? . ,(primitive 'pair?))
      (symbol? . ,(primitive 'symbol?))
      (number? . ,(primitive 'number?))
      (procedure? . ,(primitive 'procedure?))
      (vector? . ,(primitive 'vector?))
      (vector . ,(primitive 'vector))
      (vector-length . ,(primitive 'vector-length))
      (vector-ref . ,(primitive 'vector-ref))
      (apply . ,(primitive 'apply)))))

(define (evaluate expr env) (evaluate-fo (denote expr env) env))

(define (eval-k expr env)
  (evaluate-k (direct->k k-halt (denote expr env)) #f #f '() env '()))
