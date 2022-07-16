(define (case-clause param body) (vector param body))
(define (case-clause-param cc)   (vector-ref cc 0))
(define (case-clause-body  cc)   (vector-ref cc 1))

(define (binding-pair lhs rhs)   (cons lhs rhs))
(define (binding-pair-lhs bpair) (car bpair))
(define (binding-pair-rhs bpair) (cdr bpair))

(define (provenance-combine pv1 pv2)
  (cond ((not pv1)        pv2)
        ((not pv2)        pv1)
        ((equal? pv1 pv2) pv1)
        (else             (cons pv1 pv2))))

(define (ast-provenance     ast)    (vector-ref ast 1))
(define (ast-provenance-set ast pv) (let ((parts (vector->list ast)))
                                      (list->vector (cons (car parts) (cons pv (cddr parts))))))
(define (ast-provenance-add ast pv) (ast-provenance-set ast (provenance-combine pv (ast-provenance ast))))

(define (ast:quote       pv v)                     `#(quote       ,pv ,v))
(define (ast:ref         pv address)               `#(ref         ,pv ,address))
(define (ast:if          pv ast.c ast.t ast.f)     `#(if          ,pv ,ast.c ,ast.t ,ast.f))
(define (ast:seq         pv ast.effect ast.result) `#(seq         ,pv ,ast.effect ,ast.result))
(define (ast:call        pv ast.proc . ast.args)   `#(call        ,pv ,ast.proc ,ast.args))
(define (ast:case-lambda pv clause*)               `#(case-lambda ,pv ,clause*))
(define (ast:letrec      pv bpair* body)           `#(letrec      ,pv ,bpair* ,body))
(define (ast:lambda      pv param ast.body)        (ast:case-lambda pv (list (case-clause param ast.body))))
(define (ast:let         pv bpair* body)           (apply ast:call pv (ast:lambda #f (map binding-pair-lhs bpair*) body)
                                                          (map binding-pair-rhs cdr bpair*)))
(define (ast:list        pv ast*)                  (apply ast:call pv (ast:lambda #f 'xs (ast:ref #f 'xs))
                                                          ast*))

(define (alist->env alist)
  (let ((param (map car alist)))
    (cons (cenv-extend cenv.empty param)
          ((make-renv-extend (param->rparam param) (length alist))
           renv.empty (map cdr alist)))))

(define (env-cenv env) (car env))
(define (env-renv env) (cdr env))

(define cenv.empty '())
(define renv.empty '())

(define (cenv-ref cenv.0 addr)
  (let loop ((j 0) (cenv.1 cenv.0))
    (match cenv.1
      ('() (error "unbound reference" addr))
      ((cons (cons rec? (cons variadic? addrs)) cenv)
       (let find ((i 0) (a* addrs))
         (match a*
           ('()         (loop (+ j 1) cenv))
           ((cons a a*) (if (equal? a addr)
                          (if rec?
                            (if variadic?
                              (error "recursive frame cannot be variadic" cenv.1)
                              (lambda (renv) (unbox (list-ref (list-ref renv j) i))))
                            (if variadic?
                              (lambda (renv) (list-tail (list-ref renv j) i))
                              (lambda (renv) (list-ref  (list-ref renv j) i))))
                          (find (+ i 1) a*)))))))))

(define (cenv-extend      cenv param) (cenv-extend/rec? cenv param #f))
(define (cenv-extend-rec  cenv param) (cenv-extend/rec? cenv param #t))
(define (cenv-extend/rec? cenv param rec?)
  (cons (cons rec? (let loop ((param param) (addrs '()))
                     (cond ((null?   param) (cons #f (reverse addrs)))
                           ((symbol? param) (cons #t (reverse (cons param addrs))))
                           (else            (loop (cdr param) (cons (car param) addrs))))))
        cenv))

(define (param->rparam param)
  (let loop ((param param) (rparam 0))
    (cond ((null?   param) rparam)
          ((symbol? param) (- (+ rparam 1)))
          (else            (loop (cdr param) (+ rparam 1))))))

(define (make-renv-extend rparam count)
  (and (if (< rparam 0)
         (<= (- (+ rparam 1)) count)
         (= rparam count))
       (lambda (renv args) (cons args renv))))

(define (ast-stage ast cenv)
  (let loop.full ((ast ast) (cenv cenv))
    (define (loop ast) (loop.full ast cenv))
    (match ast
      (`#(quote ,_ ,value)              (lambda (renv) value))
      (`#(ref   ,_ ,address)            (cenv-ref cenv address))
      (`#(if    ,_ ,a.c ,a.t ,a.f)      (let ((s.c (loop a.c)) (s.t (loop a.t)) (s.f (loop a.f)))
                                          (lambda (renv) (if (s.c renv) (s.t renv) (s.f renv)))))
      (`#(seq   ,_ ,a.effect ,a.result) (let ((s.effect (loop a.effect)) (s.result (loop a.result)))
                                          (lambda (renv) (s.effect renv) (s.result renv))))
      (`#(call  ,_ ,a.proc ,a*.arg)     (let ((s.proc (loop a.proc)) (s*.arg (map loop a*.arg)))
                                          (lambda (renv) (apply (s.proc renv)
                                                                (map (lambda (s) (s renv)) s*.arg)))))
      (`#(case-lambda ,_ ,cc*)
        (let ((k (foldr (lambda (cc k)
                          (let* ((param  (case-clause-param cc))
                                 (cenv   (cenv-extend cenv param))
                                 (s.body (loop.full (case-clause-body cc) cenv))
                                 (rparam (param->rparam param)))
                            (lambda (renv count args)
                              (let ((renv-extend (make-renv-extend rparam count)))
                                (if renv-extend
                                  (s.body (renv-extend renv args))
                                  (k renv count args))))))
                        (lambda (renv count args)
                          (error "arity mismatch"
                                 'given count 'expected (map case-clause-param cc*)))
                        cc*)))
          (lambda (renv)
            (lambda args (k renv (length args) args)))))
      (`#(letrec ,_ ,bpair* ,a.body)
        (let* ((param       (map binding-pair-lhs bpair*))
               (rparam      (param->rparam param))
               (renv-extend (make-renv-extend rparam (length param)))
               (s.body      (loop.full a.body (cenv-extend cenv param)))
               (cenv        (cenv-extend-rec cenv param))
               (s*.arg      (map (lambda (bp) (loop.full (binding-pair-rhs bp) cenv))
                                 bpair*)))
          (lambda (renv)
            (let* ((loc*.arg (map (lambda (_) (box (void))) s*.arg))
                   (renv     (renv-extend renv loc*.arg)))
              (for-each (lambda (loc s) (set-box! loc (s renv))) loc*.arg s*.arg)
              (s.body (renv-extend renv rparam (map unbox loc*.arg))))))))))

(define (ast-eval ast env) ((ast-stage ast (env-cenv env)) (env-renv env)))
