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

(define (closure env param* body) `(closure ,env ,param* ,body))
(define (closure? datum) (and (pair? datum) (eq? 'closure (car datum))))

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

;; TODO:
;; Add k-[PRIMITIVE-OP] for each primitive.
;;   eval-fo primitives need to be updated for this.
;;   Replace initial env primitives with lambdas that invoke the primitive ops.
;;     We'll need list->vector to implement vector in this way.

(define (k-value expr k) `(k-value ,expr ,k))
(define (k-args-clear k) `(k-args-clear ,k))
(define (k-args-push k) `(k-args-push ,k))
(define k-apply '(k-apply))
(define k-return-pop '(k-return-pop))
(define (k-return-push k return-k) `(k-return-push ,k ,return-k))
;; TODO: generalize branching to handle 'case' efficiently.
(define (k-branch k-true k-false) `(k-branch ,k-true ,k-false))
(define (k-join k) `(k-join ,k))
(define k-halt '(k-halt))

(define (direct->k k expr)
  (case (car expr)
    ((literal reference) (k-value expr k))
    ((lambda)
     (k-value `(lambda ,(cadr expr) ,(direct->k k-return-pop (caddr expr))) k))
    ((if) (direct->k (k-branch (direct->k (k-join k) (caddr expr))
                               (direct->k (k-join k) (cadddr expr)))
                     (cadr expr)))
    ((application)
     (let* ((proc-k (direct->k k-apply (cadr expr)))
            (app-k (k-args-clear
                     (list-foldl (lambda (ea k) (direct->k (k-args-push k) ea))
                                 proc-k (caddr expr)))))
       (if (eq? 'k-return-pop (car k)) app-k (k-return-push app-k k))))
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
              (apply-k proc args (list (list k-halt #f '())))) args))
    ((list->vector) (vector-fo (apply list->vector args)))
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
           k-return-pop (apply-primitive-k proc args) #f #f returns))
        ((closure? proc)
         (evaluate-k (cadddr proc) #f '()
                     (env-extend* (cadr proc) (caddr proc) args) returns))
        (else (err))))
    (err)))

(define (evaluate-k k result args env returns)
  (case (car k)
    ((k-value)
     (let* ((expr (cadr k))
            (result
              (case (car expr)
                ((literal) (cadr expr))
                ((reference) (env-ref env (cadr expr)))
                ((lambda) (procedure-fo
                            (closure env (cadr expr) (caddr expr)))))))
       (evaluate-k (caddr k) result args env returns)))
    ((k-return-push)
     (evaluate-k (cadr k) result args env
                 (cons (list (caddr k) args env) returns)))
    ((k-args-clear) (evaluate-k (cadr k) result '() env returns))
    ((k-args-push) (evaluate-k (cadr k) result (cons result args) env returns))
    ((k-apply) (apply-k result args returns))
    ((k-return-pop)
     (let ((r (car returns)))
       (evaluate-k (car r) result (cadr r) (caddr r) (cdr returns))))
    ((k-branch)
     (evaluate-k (if result (cadr k) (caddr k)) result args env returns))
    ((k-join) (evaluate-k (cadr k) result args env returns))
    ((k-halt) result)
    (else (error 'evaluate-k (format "invalid expression ~s" expr)))))

(define (evaluate expr env)
  (evaluate-k (direct->k k-halt (denote expr env)) #f '() env '()))

(define env-primitives
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
      (list->vector . ,(primitive 'list->vector))
      (vector-length . ,(primitive 'vector-length))
      (vector-ref . ,(primitive 'vector-ref))
      (apply . ,(primitive 'apply)))))

(define env-initial
  (env-extend-bindings
    env-primitives
    `((vector . ,(evaluate '(lambda arg* (list->vector arg*)) env-primitives)))))
