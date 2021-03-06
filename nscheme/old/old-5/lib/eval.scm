((provide eval env:base)
 (require test length>=? param?! bpair*?! param-names param-bind
          ctx:var ctx:set! ctx:op ctx:def
          env-get-prop env-remove* env-add* env-extend*
          defstate:empty defstate-env defstate-actions
          defstate-env-set defstate-names-add defstate-actions-add))

(define (env-add*/var env b*)
  (param?! (map car b*))
  (define (b->rib b)
    (let* ((cell (make-mvector 1 (cdr b)))
           (get (lambda ()  (mvector-ref  cell 0)))
           (set (lambda (v) (mvector-set! cell 0 v))))
      (cons (car b) (list (cons ctx:var get) (cons ctx:set! set)))))
  (env-add* (map b->rib b*) env))
(define (env-extend*/var env b*)
  (env-add*/var (env-remove* env (map car b*)) b*))

;; Evaluation
(define (eval env form)
  (cond ((pair? form)
         (let* ((p (car form)) (a* (cdr form)) (proc (eval env p)))
           (if (and (string? p) (env-get-prop env p ctx:op #f))
             (apply proc env a*) (apply proc (eval* env a*)))))
        ((string? form) ((or (env-get-prop env form ctx:var #f)
                             (error '"unbound variable:" form))))
        ((or (boolean? form) (number? form)) form)
        ((procedure? form)                   (form env))
        (#t                                  (error '"invalid syntax:" form))))
(define (eval* env form*) (map (lambda (f) (eval env f)) form*))

(define (@lambda env param . body)
  (let ((cenv (env-remove* env (param-names param))))
    (lambda a (@body* (env-add*/var cenv (param-bind param a)) body))))
(define (@quote env d) d)
(define (@if env c t f) (if (eval env c) (eval env t) (eval env f)))
(define (@reset env . body) (reset (@body* env body)))
(define (@shift env p . body)
  (shift k (apply @let env (list (list p (lambda (_) k))) body)))
(define (@set! env param arg)
  (for-each (lambda (b) ((or (env-get-prop env (car b) ctx:set! #f)
                             (error '"identifier cannot be set!:" (car b)))
                         (cdr b)))
            (param-bind param (eval env arg))) #t)
(define (@letrec env b* . body)
  (bpair*?! b*)
  (define (k env) (begin (for-each (lambda (b) (apply @set! env b)) b*)
                         (apply @let env '() body)))
  (@let env (map (lambda (b) (list (car b) #t)) b*) k))
(define (@let/ env b* . body)
  (apply (apply @lambda env (map car b*) body) (eval* env (map cadr b*))))
(define (@let/name env name b* . body)
  (define p (lambda (env) (apply @lambda env (map car b*) body)))
  (apply (@letrec env (list (list name p)) name) (eval* env (map cadr b*))))
(define (@let* env b* . body)
  (let loop ((b* b*) (env env))
    (if (null? b*) (@body* env body)
      (@let/ env (list (car b*)) (lambda (env) (loop (cdr b*) env))))))
(define (@let env . tail)
  (cond ((and (length>=? 2 tail) (string? (car tail)) (bpair*?! (cadr tail)))
         (apply @let/name env tail))
        (#t (bpair*?! (car tail)) (apply @let/ env tail))))
(define (@begin env . body) (foldl (lambda (form _) (eval env form)) #t body))
(define (@and env . body)
  (foldl (lambda (form v) (and v (eval env form))) #t body))
(define (@or env . body)
  (foldl (lambda (form v) (or v (eval env form))) #f body))
(define (@when env c . body)   (if (eval env c) (@body* env body) #t))
(define (@unless env c . body) (if (eval env c) #t (@body* env body)))
(define (@cond env . clauses)
  (or (null? clauses) (if (eval env (caar clauses)) (@body* env (cdar clauses))
                        (apply @cond env (cdr clauses)))))

(define (defstate-actions-add-expr st form)
  (defstate-actions-add st (lambda (env) (eval env form))))
(define (defstate-run st)
  (define env (defstate-env st))
  (foldl (lambda (act _) (act env)) #t (reverse (defstate-actions st))))
(define (@begin/define st . forms)
  (foldl (lambda (form st)
           (let* ((n (and (pair? form) (string? (car form)) (car form)))
                  ($def (and n (env-get-prop (defstate-env st) n ctx:def #f))))
             (if $def (apply $def st (cdr form))
               (defstate-actions-add-expr st form)))) st forms))
(define (@def st param arg)
  (define names (param-names param))
  (defstate-actions-add
    (defstate-env-set
      (defstate-names-add st names)
      (env-extend*/var (defstate-env st) (map (lambda (n) (cons n #t)) names)))
    (lambda (env) (@set! env param arg))))
(define (@define st param . body)
  (if (pair? param)
    (@define st (car param) (lambda (e) (apply @lambda e (cdr param) body)))
    (@def st param (car body))))
(define (@body* env body)
  (defstate-run (apply @begin/define (defstate:empty env) body)))

;; Base environment construction
(define (ref-proc p) (cons ctx:var (lambda () p)))

(define env:base
  (append
    (map (lambda (b) (cons (car b) (list (ref-proc (cdr b))
                                         (cons ctx:op #t))))
         (list (cons 'if     @if)
               (cons 'reset  @reset)
               (cons 'shift  @shift)
               (cons 'lambda @lambda)
               (cons '$ (lambda (env rator . rands)
                          (apply (eval env rator) (cons env rands))))))
    (map (lambda (b) (cons (car b) (list (ref-proc (cdr b)))))
         (list (cons 'eval        eval)
               (cons 'apply       apply)
               (cons 'procedure?  procedure?)
               (cons 'mvector?    mvector?)
               (cons 'vector?     vector?)
               (cons 'pair?       pair?)
               (cons 'null?       null?)
               (cons 'boolean?    boolean?)
               (cons 'string?     string?)
               (cons 'number?     number?)
               (cons 'integer?    integer?)
               (cons 'fixnum?     fixnum?)
               (cons 'flonum?     flonum?)
               (cons 'boolean=?   boolean=?)
               (cons 'number=?    number=?)
               (cons 'string=?    string=?)
               (cons 'mvector=?   mvector=?)
               (cons 'procedure=? procedure=?)
               (cons 'string->vector string->vector)
               (cons 'vector->string vector->string)
               (cons 'cons cons)
               (cons 'car  car)
               (cons 'cdr  cdr)
               (cons 'vector-ref    vector-ref)
               (cons 'vector-length vector-length)
               (cons 'make-mvector    make-mvector)
               (cons 'mvector->vector mvector->vector)
               (cons 'mvector-set!    mvector-set!)
               (cons 'mvector-ref     mvector-ref)
               (cons 'mvector-length  mvector-length)
               (cons 'string<?        string<?)
               (cons 'string>?        string>?)
               (cons '=  =)
               (cons '<= <=)
               (cons '<  <)
               (cons '>= >=)
               (cons '>  >)
               (cons '+  +)
               (cons '*  *)
               (cons '-  -)
               (cons '/  /)
               (cons 'truncate truncate)
               ;; TODO: these and others?
               ;bitwise-and
               ;bitwise-ior
               ;bitwise-xor
               ;bitwise-not
               ;bitwise-bit-set?
               ;bitwise-bit-field
               ;arithmetic-shift
               ;integer-length
               ;; derivable ops imported as primitives for efficiency
               (cons 'error         error)
               (cons 'not           not)
               (cons 'caar          caar)
               (cons 'cadr          cadr)
               (cons 'cdar          cdar)
               (cons 'cadar         cadar)
               (cons 'caddr         caddr)
               (cons 'list-tail     list-tail)
               (cons 'list-ref      list-ref)
               (cons 'list->vector  list->vector)
               (cons 'vector->list  vector->list)
               (cons 'equal?        equal?)
               (cons 'vector        vector)
               (cons 'list?         list?)
               (cons 'list          list)
               (cons 'list*         list*)
               (cons 'remove        remove)
               (cons 'length        length)
               (cons 'append        append)
               (cons 'reverse       reverse)
               (cons 'range         range)
               (cons 'take          take)
               (cons 'drop          drop)
               (cons 'member        member)
               (cons 'assoc         assoc)
               (cons 'alist-get     alist-get)
               (cons 'alist-remove* alist-remove*)
               (cons 'string-append string-append)
               (cons 'foldl         foldl)
               (cons 'foldr         foldr)
               (cons 'map           map)
               (cons 'for-each      for-each)
               (cons 'andmap        andmap)
               (cons 'ormap         ormap)
               (cons 'filter        filter)
               (cons 'filter-not    filter-not)
               (cons 'remf          remf)
               (cons 'memf          memf)
               ))
    ;; These don't have to be primitive, but are provided for convenience.
    (list (cons 'define (list (cons ctx:def @define)))
          (cons 'def    (list (cons ctx:def @def)))
          (cons 'begin  (list (ref-proc @begin) (cons ctx:op #t)
                              (cons ctx:def @begin/define))))
    (map (lambda (b) (cons (car b) (list (ref-proc (cdr b)) (cons ctx:op #t))))
         (list (cons 'quote  @quote)
               (cons 'set!   @set!)
               (cons 'let    @let)
               (cons 'let*   @let*)
               (cons 'letrec @letrec)
               (cons 'and    @and)
               (cons 'or     @or)
               (cons 'when   @when)
               (cons 'unless @unless)
               (cons 'cond   @cond)))))

(when test
  (define (ev code) (eval env:base code))

  (test '$-1 ;; $ applies a procedure with current environment and raw syntax.
    (ev '($ (lambda (env . tree) tree) 4 5))
    '(4 5))
  (test '$-2 ;; We can even apply such procedures with improper argument lists.
    (ev '($ (lambda (env . tree) tree) 4 . 5))
    '(4 . 5))
  (test '$-3
    (ev '($ (lambda (env) (((cdr (assoc 'ref (cdr (assoc 'vector env)))))
                           1 2 3))))
    '#(1 2 3))

  (test 'apply-lambda-1
    (ev '((apply lambda (cons '()        ;; empty env
                              '(_ 5))))) ;; quote is unbound
    5)
  (test 'apply-lambda-2
    (ev (list (list 'apply 'lambda
                    (list 'cons (lambda (env) env) ;; spliced evaluator
                          ''(_ '5)))))             ;; quote is bound
    5)
  (test 'apply-lambda-3
    (ev '((apply lambda (cons ($ (lambda (env) env)) ;; evaluator via $
                              '(_ '5)))))            ;; quote is bound
    5)
  (test 'apply-lambda-4
    (ev '((((lambda (x) x) lambda) ;; another applicative lambda example
           ($ (lambda (env) env)) '_ ''5)))
    5))
