;; See: http://okmij.org/ftp/Computation/having-effect.html
(load "common.scm")

(define (done datum) (vector 'done datum))
(define (done? v) (eq? 'done (vector-ref v 0)))
(define (done-datum d) (vector-ref d 1))

(define (request details k) (vector 'request details k))
(define (request? v) (eq? 'request (vector-ref v 0)))
(define (request-details r) (vector-ref r 1))
(define (request-k r) (vector-ref r 2))

(define (process-requests c k)
  (if (request? c)
    (request (request-details c)
             (lambda (v) (process-requests ((request-k c) v) k)))
    (k (done-datum c))))

(define (e-literal datum) (vector 'lit datum))
(define (e-literal? e) (eq? 'lit (vector-ref e 0)))
(define (e-literal-datum e) (vector-ref e 1))

(define (e-app operator operands) (vector 'app operator operands))
(define (e-app* operator . operands) (e-app operator operands))
(define (e-app? e) (eq? 'app (vector-ref e 0)))
(define (e-app-operator e) (vector-ref e 1))
(define (e-app-operands e) (vector-ref e 2))

(define (e-if c t f) (vector 'if c t f))
(define (e-if? e) (eq? 'if (vector-ref e 0)))
(define (e-if-condition e) (vector-ref e 1))
(define (e-if-true e) (vector-ref e 2))
(define (e-if-false e) (vector-ref e 3))

(define (err e) (request `(error ,e) (lambda (_) (compute-error e))))
(define (compute-error _) err)

(define (compute-literal base)
  (lambda (compute)
    (lambda (e) (if (e-literal? e) (done (e-literal-datum e)) (base e)))))

(define (compute-app base)
  (lambda (compute)
    (lambda (e)
      (if (e-app? e)
        (process-requests
          (compute (e-app-operator e))
          (lambda (operator)
            (let loop ((operands (e-app-operands e)) (args '()))
              (if (null? operands)
                (operator (reverse args))
                (process-requests
                  (compute (car operands))
                  (lambda (arg) (loop (cdr operands) (cons arg args))))))))
        (base e)))))
(define (compute-if base)
  (lambda (compute)
    (lambda (e)
      (if (e-if? e)
        (process-requests
          (compute (e-if-condition e))
          (lambda (condition)
            (process-requests
              (compute (if condition (e-if-true e) (e-if-false e)))
              done)))
        (base e)))))

(define (e-ref name) (vector 'ref name))
(define (e-ref? e) (eq? 'ref (vector-ref e 0)))
(define (e-ref-name e) (vector-ref e 1))

(define (e-lambda p* body) (vector 'lambda p* body))
(define (e-lambda? e) (eq? 'lambda (vector-ref e 0)))
(define (e-lambda-p* e) (vector-ref e 1))
(define (e-lambda-body e) (vector-ref e 2))

(define (req-env-ref name) `(env-ref ,name))
(define (req-env-ref? req)
  (and (pair? req) (eq? 'env-ref (car req))
       (pair? (cdr req)) (null? (cddr req))))
(define (req-env-ref-name req) (cadr req))
(define req-env-lambda 'env-lambda)
(define (req-env-lambda? req) (eq? 'env-lambda req))

(define (compute-lambda base)
  (lambda (compute)
    (lambda (e)
      (cond
        ((e-ref? e) (request (req-env-ref (e-ref-name e)) done))
        ((e-lambda? e)
         (request
           req-env-lambda
           (lambda (cenv)
             (done
               (lambda (args)
                 ((respond-env (env-extend* cenv (e-lambda-p* e) args))
                  (compute (e-lambda-body e))))))))
        (else (base e))))))

(define (delegate-response c respond)
  (request (request-details c) (lambda (v) (respond ((request-k c) v)))))

(define (respond-env env)
  (lambda (c)
    (cond
      ((done? c) c)
      ((req-env-ref? (request-details c))
       ((respond-env env)
        ((request-k c)
         (env-ref env (env-address
                        env (req-env-ref-name (request-details c)))))))
      ((req-env-lambda? (request-details c))
       ((respond-env env) ((request-k c) env)))
      (else (delegate-response c (respond-env env))))))

(define e-get (vector 'get))
(define (e-get? e) (eq? 'get (vector-ref e 0)))
(define (e-put e) (vector 'put e))
(define (e-put? e) (eq? 'put (vector-ref e 0)))
(define (e-put-arg e) (vector-ref e 1))

(define req-state-get 'state-get)
(define (req-state-get? req) (eq? 'state-get req))
(define (req-state-put v) `(state-put ,v))
(define (req-state-put? req) (and (pair? req) (eq? 'state-put (car req))
                                  (pair? (cdr req)) (null? (cddr req))))
(define (req-state-put-value req) (cadr req))

(define (compute-state base)
  (lambda (compute)
    (lambda (e)
      (cond
        ((e-get? e) (request req-state-get done))
        ((e-put? e) (process-requests
                      (compute (e-put-arg e))
                      (lambda (arg) (request (req-state-put arg) done))))
        (else (base e))))))

(define (respond-state state)
  (lambda (c)
    (cond
      ((done? c) c)
      ((req-state-get? (request-details c))
       ((respond-state state) ((request-k c) state)))
      ((req-state-put? (request-details c))
       (let ((value (req-state-put-value (request-details c))))
         ((respond-state value) ((request-k c) value))))
      (else (delegate-response c (respond-state state))))))

(define (extend-compute c base)
  (lambda (compute) ((c (base compute)) compute)))
(define (extend*-compute . cs)
  (if (null? cs)
    compute-error
    (extend-compute (car cs) (apply extend*-compute (cdr cs)))))
(define (fix-compute c)
  (letrec ((compute (lambda (e) ((c compute) e)))) compute))

(define e-inc (vector 'inc))
(define (e-inc? e) (eq? 'inc (vector-ref e 0)))
(define (apply-inc args)
  (if (and (= 1 (length args)) (number? (car args)))
    (done (+ 1 (car args)))
    (err `(invalid arguments to inc: ,args))))

(define (compute-inc base)
  (lambda (compute)
    (lambda (e)
      (if (e-inc? e) (done apply-inc) (base e)))))

(define e-equal (vector '=))
(define (e-equal? e) (eq? '= (vector-ref e 0)))
(define (apply-equal args)
  (if (= 2 (length args))
    (done (apply equal? args))
    (err `(invalid arguments to =: ,args))))

(define (compute-equal base)
  (lambda (compute)
    (lambda (e)
      (if (e-equal? e) (done apply-equal) (base e)))))

(define compute
  (fix-compute
    (extend*-compute
      compute-state
      compute-lambda
      compute-equal
      compute-inc
      compute-if
      compute-app
      compute-literal)))

(define (ev e) ((respond-state #f) ((respond-env env-empty) (compute e))))


;; 4
(define tinc (e-app* e-inc (e-app* e-inc (e-literal 2))))
;; 5
(define tif (e-if (e-app* e-equal (e-literal 3) tinc)
                  (e-literal 10)
                  (e-app* e-inc tinc)))
;; 5
(define ts (e-if (e-app* e-equal (e-literal 3) (e-put tinc))
                 (e-put (e-literal 10))
                 (e-put (e-app* e-inc e-get))))
(define th0 (e-lambda '(x) (e-app* e-inc (e-ref 'x))))
;; 2
(define th1 (e-app*
              (e-lambda '(z x) (e-app* (e-ref 'z) (e-ref 'x)))
              e-inc (e-literal 1)))


;; TODO: parse : syntax -> expr
