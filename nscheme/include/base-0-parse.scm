(define (parse-binding-pairs e.bpairs)
  (define (parse-binding-pair e.bpair)
    (let ((e* (syntax->list e.bpair)))
      (unless (= (length e*) 2) (raise-syntax-error "binding pair without 2 elements" e.bpair))
      (unless (identifier? (car e*)) (raise-syntax-error "not an identifier" (car e*) e.bpair))
      (cons (car e*) (cadr e*))))
  (map parse-binding-pair (syntax->list e.bpairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-quote        env e)           (ast:quote (syntax-provenance e) (syntax->datum e)))
(define (parse-quote-syntax env e)           (ast:quote (syntax-provenance e) e))
(define (parse-if           env e.c e.t e.f) ($if (parse env e.c) (parse env e.t) (parse env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (parse-case-lambda env . e*.cc)
  (define (parse-case-lambda-clause e.cc)
    (let ((e* (syntax->list e.cc)))
      (when (or (null? e*) (null? (cdr e*))) (raise-syntax-error "not a case-lambda clause" e.cc))
      (let ((param*~ (syntax->improper-list (car e*))))
        (define (addr*->body . addr*)
          (parse-body (env-extend-scope env (improper-list->list param*~) addr*)
                      (cdr e*)))
        ($case-lambda-clause param*~ addr*->body))))
  (ast:case-lambda #f (map parse-case-lambda-clause e*.cc)))

(define (parse-and env . e*) (apply $and (parse* env e*)))
(define (parse-or  env . e*) (apply $or  (parse* env e*)))

(define (parse-when   env e.test e . e*) (apply $when   (parse* env (cons e.test (cons e e*)))))
(define (parse-unless env e.test e . e*) (apply $unless (parse* env (cons e.test (cons e e*)))))

(define (parse-cond env clause . clause*)
  (let loop ((c* (cons clause clause*)))
    (cond ((null? c*) $void)
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (raise-syntax-error "empty clause" c))
                  (let ((e.test (car e*)) (e* (cdr e*)))
                    (cond ((expression-keyword? 'else env e.test)
                           (unless (null? e*)      (raise-syntax-error "empty else clause" c))
                           (unless (null? clause*) (raise-syntax-error "else clause is not last" c))
                           (parse-body env e*))
                          ((null? e*)
                           ($let '(test) (list (parse env e.test))
                                 (lambda (addr.test)
                                   ($if ($ref addr.test) ($ref addr.test) (loop c*)))))
                          ((expression-keyword? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (raise-syntax-error "=> is not followed by one procedure" c))
                           ($let '(test) (list (parse env e.test))
                                 (lambda (addr.test)
                                   ($if ($ref addr.test)
                                        ($call (parse env (cadr e*)) ($ref addr.test))
                                        (loop c*)))))
                          (else ($if (parse env e.test) (parse-body env e*) (loop c*))))))))))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pairs e1)) (param* (map car bpair*)))
        (define (addr->rhs&body addr)
          (define (addr*->body . addr*)
            (parse-body (env-extend-scope env (cons e0 param*) (cons addr addr*)) e*))
          (cons (list ($lambda param* addr*->body)) ($ref addr)))
        ($call ($letrec (list e0) addr->rhs&body) (parse* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pairs e0)) (param* (map car bpair*)))
        (define (addr*->body . addr*)
          (parse-body (env-extend-scope env param* addr*) (cons e1 e*)))
        ($let param* (parse* env (map cdr bpair*)) addr*->body))))

(define (parse-begin-expression env e . e*)
  (let loop ((e e) (e* e*))
    (cond ((null? e*) (parse env e))
          (else       ($begin (parse env e) (loop (car e*) (cdr e*)))))))

(define (splicing-keyword->expression-parser ^%id.splicing)
  (keyword-expression-parser (lambda (env stx.def* . stx*)
                               (parse env `(,%let () (,(^%id.splicing) ,stx.def* (,%let () . ,stx*)))))
                             2 #f))

(define ((definition-keyword->expression-parser ^%id.def) env . stx*)
  (parse env `(,%let () (,(^%id.def) . ,stx*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-body env stx.body)
  (let ((stx* (syntax->list stx.body)))
    (cond ((null? stx*)       (raise-syntax-error "no expression" stx.body))
          ((null? (cdr stx*)) (parse env (car stx*)))
          (else (let* ((env.scope (make-env))
                       (env       (env-extend env env.scope))
                       (dst       (foldl (lambda (stx dst) (parse-definition dst env.scope env stx))
                                         defstate.empty stx*)))
                  (unless (defstate-expression dst)
                    (raise-syntax-error "no expression after definitions" stx.body))
                  (ast:letrec (syntax-provenance stx.body)
                              (definitions->binding-pairs (defstate-definitions dst))
                              ((defstate-expression dst))))))))

(define (parse-begin-definition dst env.scope env . stx*)
  (foldl (lambda (stx dst) (parse-definition dst env.scope env stx)) dst stx*))

(define (parse-introduce dst env.scope env . stx*) (env-introduce* env.scope stx*) dst)

(define (parse-define dst env.scope env lhs . stx*.rhs)
  (let loop ((lhs lhs) (stx*.rhs stx*.rhs))
    (cond ((identifier? lhs)
           (unless (and (pair? stx*.rhs) (null? (cdr stx*.rhs)))
             (raise-syntax-error "not a single expression" stx*.rhs))
           (let ((addr (env-introduce env.scope lhs)))
             (env-set! env.scope vocab.expression addr (parse-variable-ref/address addr))
             (defstate-define dst addr (lambda () (parse env (car stx*.rhs))))))
          (else (let ((x (syntax-unwrap lhs)))
                  (cond ((pair? x) (loop (car x) (list `(,%lambda ,(cdr x) . ,stx*.rhs))))
                        (else      (raise-syntax-error "not a definable form" lhs))))))))

(define (parse-define-values dst env.scope env stx.lhs* e.rhs)
  (let* ((lhs*        (syntax->list stx.lhs*))
         (%vec.value* (fresh-identifier 'vec.value*))
         (%rhs        (expression-parser
                        (lambda (env _)
                          ($pcall call-with-values ($lambda '() (lambda () (parse env e.rhs)))
                                  ($case-lambda
                                    (cons lhs* (lambda addr*
                                                 (apply $pcall vector (map $ref addr*))))))))))
    (parse-definition
      dst env.scope env
      `(,%begin
         (,%define ,%vec.value* ,%rhs)
         . ,(map (lambda (i lhs)
                   `(,%define ,lhs (expression-parser
                                     (lambda (env _)
                                       ($pcall vector-ref (parse env %vec.value*) ($quote i))))))
                 (iota (length lhs*)) lhs*)))))

(define (parse-splicing-local dst env.scope env stx.def* . stx*)
  (let* ((stx*.local-body (syntax->list stx.def*))
         (env.scope.inner (make-env))
         (env             (env-extend env env.scope.inner))
         (dst             (apply parse-begin-definition dst env.scope.inner env stx*.local-body)))
    (apply parse-begin-definition dst env.scope env stx*)))

(define (parse-splicing-let dst env.scope env stx.bpair* . stx*)
  (let ((bpair* (parse-binding-pairs stx.bpair*)))
    (let* ((env.scope.inner (make-env))
           (dst (foldl (lambda (lhs rhs dst) (parse-define dst env.scope.inner env lhs rhs))
                       dst (map car bpair*) (map cdr bpair*))))
      (apply parse-begin-definition dst env.scope (env-extend env env.scope.inner) stx*))))

(define (parse-splicing-let-values dst env.scope env stx.bpair* . stx*)
  (let ((bpair* (parse-binding-pairs stx.bpair*)))
    (let* ((env.scope.inner (make-env))
           (dst (foldl (lambda (lhs rhs dst) (parse-define-values dst env.scope.inner env lhs rhs))
                       dst (map car bpair*) (map cdr bpair*))))
      (apply parse-begin-definition dst env.scope (env-extend env env.scope.inner) stx*))))

(define (parse-splicing-let* dst env.scope env stx.bpair* . stx*)
  (parse-definition
    dst env.scope env
    (let loop ((bpair* (parse-binding-pairs stx.bpair*)))
      (cond ((null? bpair*) `(%splicing-let ()              . ,stx*))
            (else           `(%splicing-let (,(car bpair*)) ,(loop (cdr bpair*))))))))

(define (parse-splicing-let*-values dst env.scope env stx.bpair* . stx*)
  (parse-definition
    dst env.scope env
    (let loop ((bpair* (parse-binding-pairs stx.bpair*)))
      (cond ((null? bpair*) `(%splicing-let-values ()              . ,stx*))
            (else           `(%splicing-let-values (,(car bpair*)) ,(loop (cdr bpair*))))))))

(define (parse-splicing-letrec* dst env.scope env stx.bpair* . stx*)
  (let ((bpair* (parse-binding-pairs stx.bpair*)))
    (parse-definition dst env.scope env
                      `(,%splicing-local ,(map (lambda (lhs rhs) `(,%define ,lhs ,rhs))
                                               (map car bpair*) (map cdr bpair*))
                                         . ,stx*))))

(define (parse-splicing-letrec*-values dst env.scope env stx.bpair* . stx*)
  (let ((bpair* (parse-binding-pairs stx.bpair*)))
    (parse-definition dst env.scope env
                      `(,%splicing-local ,(map (lambda (lhs rhs) `(,%define-values ,lhs ,rhs))
                                               (map car bpair*) (map cdr bpair*))
                                         . ,stx*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment and qualified identifiers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env.base-0
  (let ((env.scope (make-env))
        (primitive*
          (list
            procedure-metadata procedure-metadata-set!
            eq? eqv? eof-object? null? procedure? pair? cons car cdr
            string->symbol symbol->string symbol? string? vector vector? vector-length vector-ref
            vector->svector svector->vector svector svector? svector-length svector-ref
            mvector->vector mvector make-mvector mvector? mvector-length mvector-ref mvector-set!
            utf8->string string->utf8 bytevector bytevector? bytevector-length bytevector-ref
            mbytevector->bytevector make-mbytevector mbytevector mbytevector?
            mbytevector-length mbytevector-ref mbytevector-set!
            number? exact? integer? inexact? = <= < + - * / quotient remainder truncate
            integer-length bitwise-arithmetic-shift << >> & \| ^))
        (b*.expr-aux '(=> else))
        (b*.def
          (list (cons 'define        (keyword-definition-parser parse-define        2 #f))
                (cons 'define-values (keyword-definition-parser parse-define-values 2 #f))
                (cons 'introduce     (keyword-definition-parser parse-introduce     0 #f))))
        (b*.expr
          (list
            (cons 'quote          (keyword-expression-parser parse-quote          1 1))
            (cons 'quote-syntax   (keyword-expression-parser parse-quote-syntax   1 1))
            (cons 'if             (keyword-expression-parser parse-if             3 3))
            (cons 'and            (keyword-expression-parser parse-and            0 #f))
            (cons 'or             (keyword-expression-parser parse-or             0 #f))
            (cons 'when           (keyword-expression-parser parse-when           2 #f))
            (cons 'unless         (keyword-expression-parser parse-unless         2 #f))
            (cons 'cond           (keyword-expression-parser parse-cond           1 #f))
            (cons 'case-lambda    (keyword-expression-parser parse-case-lambda    0 #f))
            (cons 'lambda         (keyword-expression-parser parse-lambda         2 #f))
            (cons 'local          (splicing-keyword->expression-parser (lambda () %splicing-local)))
            (cons 'let            (keyword-expression-parser parse-let            2 #f))
            (cons 'let*           (splicing-keyword->expression-parser (lambda () %splicing-let*)))
            (cons 'letrec*        (splicing-keyword->expression-parser (lambda () %splicing-letrec*)))
            (cons 'letrec         (splicing-keyword->expression-parser (lambda () %splicing-letrec)))
            (cons 'let-values     (splicing-keyword->expression-parser (lambda () %splicing-let-values)))
            (cons 'let*-values    (splicing-keyword->expression-parser (lambda () %splicing-let*-values)))
            (cons 'letrec*-values (splicing-keyword->expression-parser (lambda () %splicing-letrec*-values)))
            (cons 'letrec-values  (splicing-keyword->expression-parser (lambda () %splicing-letrec-values)))))
        (b*.def-and-expr
          (list
            (list 'begin
                  (keyword-definition-parser parse-begin-definition 0 #f)
                  (keyword-expression-parser parse-begin-expression 1 #f))
            (list 'splicing-local
                  (keyword-definition-parser parse-splicing-local 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-local)))
            (list 'splicing-let
                  (keyword-definition-parser parse-splicing-let 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-let)))
            (list 'splicing-let*
                  (keyword-definition-parser parse-splicing-let* 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-let*)))
            (list 'splicing-letrec*
                  (keyword-definition-parser parse-splicing-letrec* 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-letrec*)))
            (list 'splicing-letrec
                  (keyword-definition-parser parse-splicing-letrec* 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-letrec)))
            (list 'splicing-let-values
                  (keyword-definition-parser parse-splicing-let-values 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-let-values)))
            (list 'splicing-let*-values
                  (keyword-definition-parser parse-splicing-let*-values 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-let*-values)))
            (list 'splicing-letrec*-values
                  (keyword-definition-parser parse-splicing-letrec*-values 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-letrec*-values)))
            (list 'splicing-letrec-values
                  (keyword-definition-parser parse-splicing-letrec*-values 2 #f)
                  (definition-keyword->expression-parser (lambda () %splicing-letrec-values))))))
    (for-each (lambda (id)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.expression:keyword addr (syntax-peek id))))
              b*.expr-aux)
    (for-each (lambda (p) (let* ((pm   (procedure-metadata p))
                                 (id   (if (and (vector? pm)
                                                (= (vector-length pm) 2)
                                                (eq? (vector-ref pm 0) 'primitive))
                                           (vector-ref pm 1)
                                           (error "not a primitive" p)))
                                 (addr (identifier->fresh-address id)))
                            (env-bind! env.scope id addr)
                            (env-set!  env.scope vocab.expression addr
                                       (parse-variable-ref/address addr))))
              primitive*)
    (for-each (lambda (id op)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.definition:operator addr op)))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id op.def op.expr)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.definition:operator addr op.def)
                  (env-set!  env.scope vocab.expression:operator addr op.expr)))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id op)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.expression:operator addr op)))
              (map car b*.expr) (map cdr b*.expr))
    (env-extend env.empty env.scope)))

(define-values
  (%=> %else %begin %quote %quote-syntax %if %and %or %when %unless %cond
       %case-lambda %lambda %local
       %let %let* %letrec* %letrec %let-values %let*-values %letrec*-values %letrec-values
       %define %define-values %introduce
       %splicing-local %splicing-let %splicing-let* %splicing-letrec* %splicing-letrec
       %splicing-let-values %splicing-let*-values
       %splicing-letrec*-values %splicing-letrec-values)
  (apply
    values
    (syntax->list
      (syntax-qualify
        '(=> else begin quote quote-syntax if and or when unless cond
             case-lambda lambda local
             let let* letrec* letrec let-values let*-values letrec*-values letrec-values
             define define-values introduce
             splicing-local splicing-let splicing-let* splicing-letrec* splicing-letrec
             splicing-let-values splicing-let*-values
             splicing-letrec*-values splicing-letrec-values)
        env.base-0))))
