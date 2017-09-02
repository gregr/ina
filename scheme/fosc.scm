;; See: https://themonadreader.files.wordpress.com/2014/04/super-final.pdf

;; P ::= ((C1 arity1) ... (Cn arityn)) d1 ... dn
;; d ::= (define (f v1 ... vn) e)
;;     | (define (g p1 v1 ... vn) e1)
;;       ...
;;       (define (g pm v1 ... vn) em)
;; e ::= v
;;     | (C e1 ... en)
;;     | (f e1 ... en)
;; p ::= (C v1 ... vn)

(define (tagged? t v) (and (pair? v) (eq? t (car v))))

(define (env prog pa*) `(env ,prog ,pa*))
(define (env-prog e) (cadr e))
(define (env-ctor* e) (program-ctor* (env-prog e)))
(define (env-fn* e) (program-def* (env-prog e)))
(define (env-pa* e) (caddr e))
(define (env-apply e param* arg*) (env (env-prog e) (bind-param* param* arg*)))
(define (env-lookup e name)
  (or (let ((arg (env-pa e name))) (and arg (list 'pa name arg)))
      (env-fn e name)
      (let ((ctor (env-ctor e name))) (and ctor (list 'ctor ctor)))))
(define (env-ctor e name)
  (let loop ((ctor* (env-ctor* e)))
    (and (pair? ctor*)
         (if (eq? name (caar ctor*)) (car ctor*) (loop (cdr ctor*))))))
(define (env-fn e name)
  (let loop ((fn* (env-fn* e)))
    (and (pair? fn*)
         (if (eq? name (fn-name (car fn*))) (car fn*) (loop (cdr fn*))))))
(define (env-pa e name)
  (let loop ((pa* (env-pa* e)))
    (and (pair? pa*)
         (if (eq? name (caar pa*)) (cadar pa*) (loop (cdr pa*))))))

(define (bind-param* param* arg*)
  (cond
    ((and (null? param*) (null? arg*)) '())
    ((and (pair? param*) (pair? arg*))
     (cons `(,(car param*) ,(car arg*)) (bind-param* (cdr param*) (cdr arg*))))
    (else (error 'bind-param* (format "invalid binding: ~s ~s" param* arg*)))))

(define (program ctor* def*) `(program ,ctor* ,def*))
(define (program? v) (tagged? 'program v))
(define (program-ctor* p) (cadr p))
(define (program-def* p) (caddr p))

(define (fn-indifferent name param* body) `(fn-i ,name ,param* ,body))
(define (fn-indifferent? v) (tagged? 'fn-i v))
(define (fn-indifferent-name fn) (cadr fn))
(define (fn-indifferent-param* fn) (caddr fn))
(define (fn-indifferent-body fn) (cadddr fn))

(define (fn-curious name clause*) `(fn-c ,name ,clause*))
(define (fn-curious? v) (tagged? 'fn-c v))
(define (fn-curious-name fn) (cadr fn))
(define (fn-curious-clause* fn) (caddr fn))
(define (fn-curious-extend fn clause*)
  (fn-curious (fn-curious-name fn) (append clause* (fn-curious-clause* fn))))

(define (fn-name fn)
  (or (and (fn-indifferent? fn) (fn-indifferent-name fn))
      (and (fn-curious? fn) (fn-curious-name fn))))

(define (p-clause pattern param* body) `(p-clause ,pattern ,param* ,body))
(define (p-clause? v) (tagged? 'p-clause v))
(define (p-clause-pattern c) (cadr c))
(define (p-clause-param* c) (caddr c))
(define (p-clause-body c) (cadddr c))

(define (e-cons c ea*) `(e-cons ,c ,ea*))
(define (e-cons? v) (tagged? 'e-cons v))
(define (e-cons-c e) (cadr e))
(define (e-cons-ea* e) (caddr e))
(define (e-app f ea*) `(e-app ,f ,ea*))
(define (e-app? v) (tagged? 'e-app v))
(define (e-app-f e) (cadr e))
(define (e-app-ea* e) (caddr e))
(define (e-var name) `(e-var ,name))
(define (e-var? v) (tagged? 'e-var v))
(define (e-var-name e) (cadr e))

(define (pattern c n*) `(pattern ,c ,n*))
(define (pattern? v) (tagged? 'pattern v))
(define (pattern-ctor p) (cadr p))
(define (pattern-param* p) (caddr p))


(define (parse-program stx)
  (or (and (pair? stx)
           (let* ((ctor* (parse-ctor* (car stx)))
                  (p (program ctor* (parse-def* ctor* (cdr stx))))
                  (_ (validate-program p)))
             p))
      (error 'parse-program (format "invalid program: ~s" stx))))

(define (parse-ctor* stx)
  (or (null? stx)
      (and (pair? stx)
           (pair? (car stx))
           (symbol? (caar stx))
           (pair? (cdar stx))
           (number? (cadar stx))
           (null? (cddar stx))
           (parse-ctor* (cdr stx))
           (not (assoc (caar stx) (cdr stx)))
           stx)
      (error 'parse-ctor* (format "invalid constructor list: ~s" stx))))

(define (parse-def* ctor* stx)
  (define (parse-def stx)
    (if (and (tagged? 'define stx)
             (pair? (cdr stx))
             (pair? (cddr stx))
             (null? (cdddr stx))
             (pair? (cadr stx))
             (symbol? (caadr stx)))
      (if (pair? (cadadr stx))
        (parse-def-curious (caadr stx) (cadadr stx) (cddadr stx) (caddr stx))
        (parse-def-indifferent (caadr stx) (cdadr stx) (caddr stx)))
      (error 'parse-def (format "invalid definition: ~s" stx))))
  (define (parse-def-indifferent name param*-stx body-stx)
    (define param* (parse-name* param*-stx))
    (define body (parse-expr (env-apply (env (program ctor* '()) '())
                                        param* param*) body-stx))
    (fn-indifferent name param* body))
  (define (parse-def-curious name pat-stx param*-stx body-stx)
    (define pattern (parse-pattern ctor* pat-stx))
    (define param* (parse-name* param*-stx))
    (define pa* (append (pattern-param* pattern) param*))
    (define body (parse-expr (env-apply (env (program ctor* '()) '()) pa* pa*)
                             body-stx))
    (fn-curious name (list (p-clause pattern param* body))))
  (define (extend def* def)
    (if (and (fn-curious? def) (pair? def*) (fn-curious? (car def*))
             (eq? (fn-curious-name def) (fn-curious-name (car def*))))
      (cons (fn-curious-extend (car def*) (fn-curious-clause* def)) (cdr def*))
      (or (and (not (env-lookup (env (program ctor* def*) '()) (fn-name def)))
               (cons def def*))
          (error 'parse-def* (format "name defined multiple times: ~s ~s"
                                     (fn-name def) stx)))))
  (cond
    ((null? stx) '())
    ((pair? stx) (extend (parse-def* ctor* (cdr stx)) (parse-def (car stx))))
    (else (error 'parse-def* (format "invalid definition list: ~s" stx)))))

(define (parse-name* name*)
  (and (or (null? name*)
           (and (pair? name*) (symbol? (car name*)) (parse-name* (cdr name*))
                (not (memq (car name*) (cdr name*))))
           (error 'parse-name* (format "invalid name list: ~s" name*)))
       name*))
(define (parse-pattern ctor* stx)
  (or (and (pair? stx) (symbol? (car stx))
           (let* ((p (pattern (car stx) (parse-name* (cdr stx))))
                  (ctor (assoc (car stx) ctor*))
                  (arity (and ctor (cadr ctor))))
             (and ctor (= arity (length (pattern-param* p))) p)))
      (error 'parse-pattern (format "invalid pattern: ~s" stx))))

(define (parse-expr e stx)
  (cond
    ((symbol? stx) (if (env-pa e stx) (e-var stx)
                     (error 'parse-expr (format "unbound variable: ~s ~s" e stx))))
    ((and (pair? stx) (symbol? (car stx)))
     (let ((arg* (map (lambda (stx) (parse-expr e stx)) (cdr stx)))
           (ctor (env-ctor e (car stx))))
       (if ctor
         (if (= (cadr ctor) (length arg*))
           (e-cons (car ctor) arg*)
           (error 'parse-expr (format "invalid constructor arity: ~s" stx)))
         (e-app (car stx) arg*))))
    (else (error 'parse-expr (format "invalid expression: ~s" stx)))))

(define (validate-program program)
  (define (validate-def fn)
    (if (fn-indifferent? fn)
      (validate-body (fn-indifferent-body fn))
      (andmap validate-body (map p-clause-body (fn-curious-clause* fn)))))
  (define (validate-body body)
    (cond
      ((e-app? body)
       (and (or (env-fn e (e-app-f body))
                (error 'validate-program
                       (format "undefined function: ~s" (print-expr body))))
            (andmap validate-body (e-cons-ea* body))))
      ((e-cons? body) (andmap validate-body (e-cons-ea* body)))
      (else #t)))
  (define e (env program '()))
  (andmap validate-def (program-def* program)))

(define (print-program program)
  (cons (program-ctor* program)
        (append* (map print-def (program-def* program)))))
(define (print-def fn)
  (define name (fn-name fn))
  (define (print-fnc c)
    (define pat (p-clause-pattern c))
    `(define (,name (,(pattern-ctor pat) . ,(pattern-param* pat))
                    . ,(p-clause-param* c))
       ,(print-expr (p-clause-body c))))
  (if (fn-indifferent? fn)
    `((define (,name . ,(fn-indifferent-param* fn))
        ,(print-expr (fn-indifferent-body fn))))
    (map print-fnc (fn-curious-clause* fn))))
(define (print-expr e)
  (cond
    ((e-var? e) (e-var-name e))
    ((e-cons? e) (cons (e-cons-c e) (map print-expr (e-cons-ea* e))))
    ((e-app? e) (cons (e-app-f e) (map print-expr (e-app-ea* e))))
    (else #f)))
