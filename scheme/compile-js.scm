(load "eval-fo.scm")
(load "js.scm")

;; TODO: strings and other types, once supported by evalo-fo.
(define (compile-literal datum)
  (cond ((pair? datum) (compile-pair (car datum) (cdr datum)))
        ((null? datum) 'null)
        ((symbol? datum) (symbol->string datum))
        ((or (boolean? datum) (number? datum)) datum)
        ((vector-fo? datum) (compile-vector (tagged-payload datum)))
        ((primitive? datum) (compile-primitive (tagged-payload datum)))
        (else (error 'compile-literal (format "invalid literal ~s" datum)))))

(define (compile-pop e) `((put result ,e) (return (app (get ks "pop") ()))))
(define (compile-push k) `(app (get ks "push") (,k)))
(define (compile-let bindings body)
  `(app (function ,(map car bindings) ,body) ,(map cadr bindings)))

(define js-false (compile-literal #f))
(define js-nil (compile-literal '()))
(define js-t-pair (compile-literal 'pair))
(define js-t-vector (compile-literal 'vector))
(define (js-cons h t) `(array (,js-t-pair (array (,h ,t)))))
(define (js-car ref) `(get (get ,ref 1) 0))
(define (js-cdr ref) `(get (get ,ref 1) 1))
(define (js-vector-from-array arr) `(array (,js-t-vector ,arr)))
(define (js-vector args) (js-vector-from-array `(array ,args)))
(define (js-vector-ref ref idx) `(get (get ,ref 1) ,idx))
(define (js-vector-length ref) `(get (get ,ref 1) "length"))
(define (js-null? v) `(=== ,js-nil ,v))
(define (js-pair? v)
  `(land (land (=== "object" (typeof ,v)) (!== ,js-nil ,v))
         (=== ,js-t-pair (get ,v 0))))
(define (js-vector? v)
  `(land (land (=== "object" (typeof ,v)) (!== ,js-nil ,v))
         (=== ,js-t-vector (get ,v 0))))
(define (js-symbol? v) `(=== "string" (typeof ,v)))
(define (js-number? v) `(=== "number" (typeof ,v)))
(define (js-procedure? v) `(=== "function" (typeof ,v)))
(define js-===
  `(function
     () ,(compile-pop `(=== ,(js-car 'result) ,(js-car (js-cdr 'result))))))

(define (compile-pair h t) (js-cons (compile-literal h) (compile-literal t)))
(define (compile-vector vec)
  (js-vector (map compile-literal (vector->list vec))))
(define (compile-primitive name)
  (case name
    ((cons) `(function
               () ,(compile-pop (js-cons (js-car 'result)
                                         (js-car (js-cdr 'result))))))
    ((car) `(function () ,(compile-pop (js-car (js-car 'result)))))
    ((cdr) `(function () ,(compile-pop (js-cdr (js-car 'result)))))
    ((=) js-===)
    ((boolean=?) js-===)
    ((symbol=?) js-===)
    ((null?) `(function () ,(compile-pop (js-null? (js-car 'result)))))
    ((pair?) `(function () ,(compile-pop (js-pair? (js-car 'result)))))
    ((symbol?) `(function () ,(compile-pop (js-symbol? (js-car 'result)))))
    ((number?) `(function () ,(compile-pop (js-number? (js-car 'result)))))
    ((procedure?)
     `(function () ,(compile-pop (js-procedure? (js-car 'result)))))
    ((apply) `(function () ((vars ((f ,(js-car 'result))))
                            (put result ,(js-car (js-cdr 'result)))
                            (return f))))
    ((vector) `(function () ((vars ((arr (array ())) (xs result)))
                             (while (!== ,js-nil xs)
                                    ((app (get arr "push") (,(js-car 'xs)))
                                     (put xs ,(js-cdr 'xs))))
                             . ,(compile-pop (js-vector-from-array 'arr)))))
    ((vector?) `(function () ,(compile-pop (js-vector? (js-car 'result)))))
    ((vector-length)
     `(function () ,(compile-pop (js-vector-length (js-car 'result)))))
    ((vector-ref)
     `(function
        () ,(compile-pop (js-vector-ref (js-car 'result)
                                        (js-car (js-cdr 'result))))))
    (else (error 'compile-primitive (format "invalid primitive ~s" name)))))

(define (compile-env-ref address)
  (define fi (address-fi address))
  (let loop ((ref 'env) (ei (address-eidx address)))
    (if (= 0 ei)
      (let floop ((ref (js-car ref)) (fpos (frame-info-idx fi)))
        (if (= 0 fpos)
          (if (frame-info-single? fi) (js-car ref) ref)
          (floop (js-cdr ref) (- fpos 1))))
      (loop (js-cdr ref) (- ei 1)))))

(define (compile-application proc args)
  (define (compile-arg arg)
    `(,(compile-let
         '((saved_env env) (frame result))
         `(,(compile-push
              `(function
                 () ((put env saved_env)
                     . ,(compile-pop (js-cons 'result 'frame)))))))
       . ,(compile-fo arg)))
  (let loop ((args args)
             (k `(,(compile-let
                     '((frame result))
                     `(,(compile-push
                          `(function () (,(compile-push 'result)
                                          . ,(compile-pop 'frame))))))
                   . ,(compile-fo proc))))
    (if (null? args) `((put result ,js-nil) . ,k)
      (loop (cdr args)
            `(,(compile-push `(function () ,k))
               . ,(compile-arg (car args)))))))

(define (compile-fo expr)
  (case (car expr)
    ((literal) (compile-pop (compile-literal (cadr expr))))
    ((reference) (compile-pop (compile-env-ref (cadr expr))))
    ((application) (compile-application (cadr expr) (caddr expr)))
    ((if) `((return
              ,(compile-let
                 '((saved_env env))
                 `(,(compile-push
                      `(function
                         () ((put env saved_env)
                             (if/else (!== ,js-false result)
                                      ,(compile-fo (caddr expr))
                                      ,(compile-fo (cadddr expr))))))
                    . ,(compile-fo (cadr expr)))))))
    ((lambda)
     `((return
         ,(compile-let
            '((saved_env env))
            (compile-pop
              `(function
                 () ((put env ,(js-cons 'result 'saved_env))
                     . ,(compile-fo (caddr expr)))))))))
    (else (error 'compile-fo (format "invalid expression ~s" expr)))))

(define (compile expr env)
  (js-expr `(function
              () ((vars ((ks (array (,js-false)))
                         result
                         (env ,(compile-literal (map cdr env)))
                         (k (function () ,(compile-fo (denote expr env))))))
                  (while (!== ,js-false k) ((put k (app k ()))))
                  (return result)))))