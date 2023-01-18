;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-quote        env e)           (ast:quote (syntax-provenance e) (syntax->datum e)))
(define (parse-quote-syntax env e)           (ast:quote (syntax-provenance e) e))
(define (parse-if           env e.c e.t e.f) ($if (parse-expression env e.c)
                                                  (parse-expression env e.t)
                                                  (parse-expression env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (parse-case-lambda env . e*.cc)
  (define (parse-case-lambda-clause e.cc)
    (let ((e* (syntax->list e.cc)))
      (when (or (null? e*) (null? (cdr e*))) (raise-syntax-error "not a case-lambda clause" e.cc))
      ($case-lambda-clause env (syntax->improper-list (car e*))
                           (lambda (env . _) (parse-body env (cdr e*))))))
  (ast:case-lambda #f (map parse-case-lambda-clause e*.cc)))

(define (parse-and    env . e*) (apply $and    (parse-expression* env e*)))
(define (parse-or     env . e*) (apply $or     (parse-expression* env e*)))
(define (parse-when   env . e*) (apply $when   (parse-expression* env e*)))
(define (parse-unless env . e*) (apply $unless (parse-expression* env e*)))

(define (parse-cond env clause . clause*)
  (let loop ((c* (cons clause clause*)))
    (cond ((null? c*) $void)
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (raise-syntax-error "empty clause" c))
                  (let ((e.test (car e*)) (e* (cdr e*)))
                    (cond ((expression-auxiliary? 'else env e.test)
                           (unless (null? e*)      (raise-syntax-error "empty else clause" c))
                           (unless (null? clause*) (raise-syntax-error "else clause is not last" c))
                           (parse-body env e*))
                          ((null? e*)
                           ($let env '(test) (list (parse-expression env e.test))
                                 (lambda (_ addr.test)
                                   ($if ($ref addr.test) ($ref addr.test) (loop c*)))))
                          ((expression-auxiliary? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (raise-syntax-error "=> is not followed by one procedure" c))
                           ($let env '(test) (list (parse-expression env e.test))
                                 (lambda (_ addr.test)
                                   ($if ($ref addr.test)
                                        ($call (parse-expression env (cadr e*)) ($ref addr.test))
                                        (loop c*)))))
                          (else ($if (parse-expression env e.test)
                                     (parse-body env e*)
                                     (loop c*))))))))))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pairs e1)) (param* (map car bpair*)))
        ($call ($letrec env (list e0)
                        (lambda (env addr)
                          (values (list ($lambda env param* (lambda (env . _) (parse-body env e*))))
                                  ($ref addr))))
               (parse-expression* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pairs e0)) (param* (map car bpair*)))
        ($let env param* (parse-expression* env (map cdr bpair*))
              (lambda (env . _) (parse-body env (cons e1 e*)))))))

(define (parse-begin-expression env e . e*)
  (let loop ((e e) (e* e*))
    (cond ((null? e*) (parse-expression env e))
          (else       ($begin (parse-expression env e) (loop (car e*) (cdr e*)))))))

(splicing-local
  ((define (etc-splicing-expression-operator-parser $splicing stx*->^body)
     (expression-operator-parser
       (lambda (env stx.def* . stx*)
         ($body env (lambda (dst scope env) ($splicing dst scope env stx.def* (stx*->^body stx*)))))
       2 #f)))

  (define (nonsplicing-expression-operator-parser $splicing)
    (etc-splicing-expression-operator-parser $splicing (lambda (stx*) (lambda (_ __ env)
                                                                        (parse-body env stx*)))))

  (define (splicing-expression-operator-parser $splicing)
    (etc-splicing-expression-operator-parser
      $splicing (lambda (stx*) (lambda (dst scope env)
                                 (apply parse-begin-definition dst scope env stx*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-body env stx.body)
  (let ((stx* (syntax->list stx.body)))
    (cond ((null? stx*)       (raise-syntax-error "no expression" stx.body))
          ((null? (cdr stx*)) (parse-expression env (car stx*)))
          (else (define (^def dst scope env)
                  (let ((dst (foldl (lambda (s dst) (parse-definition dst scope env s)) dst stx*)))
                    (unless (defstate-expression dst)
                      (raise-syntax-error "no expression after definitions" stx.body))
                    dst))
                (ast-provenance-add ($body env ^def) (syntax-provenance stx.body))))))

(define (parse-begin-definition dst env.scope env . stx*)
  (foldl (lambda (stx dst) (parse-definition dst env.scope env stx)) dst stx*))

(define (parse-introduce dst env.scope env . stx*) (env-introduce*! env.scope stx*) dst)

(define (parse-introduce-alias dst env.scope env id.lhs id.rhs)
  (parse-undefined-identifier env.scope id.lhs)
  (parse-identifier id.rhs)
  (let ((v=>v (env-ref env id.rhs)))
    (unless v=>v (raise-syntax-error "unbound identifier" id.rhs))
    (env-set! env.scope id.lhs v=>v))
  dst)

(define (parse-define dst env.scope env lhs . stx*.rhs)
  (cond
    ((identifier? lhs)
     (unless (and (pair? stx*.rhs) (null? (cdr stx*.rhs)))
       (raise-syntax-error "not a single expression" stx*.rhs))
     ($define dst env.scope lhs (lambda () (parse-expression env (car stx*.rhs)))))
    (else (let loop ((lhs lhs) (^rhs (lambda (env) (parse-body env stx*.rhs))))
            (let ((x (syntax-unwrap lhs)))
              (cond ((pair?       x) (loop (car x) (lambda (env)
                                                     ($lambda env (syntax->improper-list (cdr x))
                                                              (lambda (env . _) (^rhs env))))))
                    ((identifier? x) ($define dst env.scope x (lambda () (^rhs env))))
                    (else            (raise-syntax-error "not a definable form" lhs))))))))

(define (parse-define-values dst env.scope env stx.lhs*~ e.rhs)
  (let* ((lhs*~   (syntax->improper-list stx.lhs*~))
         (lhs*    (improper-list->list lhs*~))
         (v*.^rhs (lambda ()
                    ($pcall call-with-values (ast:lambda #f '() (parse-expression env e.rhs))
                            ($lambda env lhs*~ (lambda (env . addr*)
                                                 (apply $pcall vector (map $ref addr*)))))))
         (v*.addr (fresh-address 'vec.value*))
         ($v*     ($ref v*.addr)))
    (foldl (lambda (i lhs dst)
             ($define dst env.scope lhs (lambda () ($pcall vector-ref $v* ($quote i)))))
           (defstate-define dst v*.addr v*.^rhs)
           (iota (length lhs*))
           lhs*)))

(define ($splicing-rec dst env.scope env ^def ^body)
  (let* ((env.scope.inner (make-env))
         (env             (env-extend env env.scope.inner))
         (dst             (^def dst env.scope.inner env)))
    (^body dst env.scope env)))

(define ($splicing-nonrec dst env.scope env ^def ^body)
  (let ((env.scope.inner (make-env)))
    (^body (^def dst env.scope.inner env) env.scope (env-extend env env.scope.inner))))

(define ($splicing-local dst env.scope env stx.def* ^body)
  (let ((def* (syntax->list stx.def*)))
    ($splicing-rec dst env.scope env
                   (lambda (dst scope env) (apply parse-begin-definition dst scope env def*))
                   ^body)))

(splicing-local
  ((define ($splicing $splicing-etc parse-def dst env.scope env stx.bpair* ^body)
     (let ((bpair* (parse-binding-pairs stx.bpair*)))
       ($splicing-etc dst env.scope env
                      (lambda (dst scope env)
                        (foldl (lambda (lhs rhs dst) (parse-def dst scope env lhs rhs))
                               dst (map car bpair*) (map cdr bpair*)))
                      ^body))))
  (define ($splicing-let            . a*) (apply $splicing $splicing-nonrec parse-define        a*))
  (define ($splicing-let-values     . a*) (apply $splicing $splicing-nonrec parse-define-values a*))
  (define ($splicing-letrec*        . a*) (apply $splicing $splicing-rec parse-define        a*))
  (define ($splicing-letrec*-values . a*) (apply $splicing $splicing-rec parse-define-values a*)))

(splicing-local
  ((define ($splicing $splicing-etc dst env.scope env stx.bpair* ^body)
     (let loop ((bpair* (parse-binding-pairs stx.bpair*)) (dst dst) (scope env.scope) (env env))
       (cond ((null? bpair*) (^body dst scope env))
             (else ($splicing-etc dst scope env (list (car bpair*))
                                  (lambda (dst scope env) (loop (cdr bpair*) dst scope env))))))))
  (define ($splicing-let*        . a*) (apply $splicing $splicing-let        a*))
  (define ($splicing-let*-values . a*) (apply $splicing $splicing-let-values a*)))

(splicing-local
  ((define (parse-splicing $splicing dst env.scope env stx.def* . stx*)
     ($splicing dst env.scope env stx.def*
                (lambda (dst scope env) (apply parse-begin-definition dst scope env stx*)))))
  (define (parse-splicing-local          . a*) (apply parse-splicing $splicing-local          a*))
  (define (parse-splicing-let            . a*) (apply parse-splicing $splicing-let            a*))
  (define (parse-splicing-let-values     . a*) (apply parse-splicing $splicing-let-values     a*))
  (define (parse-splicing-let*           . a*) (apply parse-splicing $splicing-let*           a*))
  (define (parse-splicing-let*-values    . a*) (apply parse-splicing $splicing-let*-values    a*))
  (define (parse-splicing-letrec*        . a*) (apply parse-splicing $splicing-letrec*        a*))
  (define (parse-splicing-letrec*-values . a*) (apply parse-splicing $splicing-letrec*-values a*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pre-base language syntax environment ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env.base-0
  (let ((env.scope (make-env))
        (b*.expr-aux '(=> else))
        (b*.def
          (list (cons 'define          (definition-operator-parser parse-define          2 #f))
                (cons 'define-values   (definition-operator-parser parse-define-values   2 #f))
                (cons 'introduce       (definition-operator-parser parse-introduce       0 #f))
                (cons 'introduce-alias (definition-operator-parser parse-introduce-alias 2 #f))))
        (b*.expr
          (list
            (cons 'quote          (expression-operator-parser parse-quote        1 1))
            (cons 'quote-syntax   (expression-operator-parser parse-quote-syntax 1 1))
            (cons 'if             (expression-operator-parser parse-if           3 3))
            (cons 'and            (expression-operator-parser parse-and          0 #f))
            (cons 'or             (expression-operator-parser parse-or           0 #f))
            (cons 'when           (expression-operator-parser parse-when         2 #f))
            (cons 'unless         (expression-operator-parser parse-unless       2 #f))
            (cons 'cond           (expression-operator-parser parse-cond         1 #f))
            (cons 'case-lambda    (expression-operator-parser parse-case-lambda  0 #f))
            (cons 'lambda         (expression-operator-parser parse-lambda       2 #f))
            (cons 'local          (nonsplicing-expression-operator-parser $splicing-local))
            (cons 'let*           (nonsplicing-expression-operator-parser $splicing-let*))
            (cons 'letrec*        (nonsplicing-expression-operator-parser $splicing-letrec*))
            (cons 'let-values     (nonsplicing-expression-operator-parser $splicing-let-values))
            (cons 'let*-values    (nonsplicing-expression-operator-parser $splicing-let*-values))
            (cons 'letrec*-values (nonsplicing-expression-operator-parser $splicing-letrec*-values))
            (cons 'let            (expression-operator-parser parse-let          2 #f))))
        (b*.def-and-expr
          (list
            (list 'begin
                  (definition-operator-parser parse-begin-definition 0 #f)
                  (expression-operator-parser parse-begin-expression 1 #f))
            (list 'splicing-local
                  (definition-operator-parser parse-splicing-local 2 #f)
                  (splicing-expression-operator-parser $splicing-local))
            (list 'splicing-let
                  (definition-operator-parser parse-splicing-let 2 #f)
                  (splicing-expression-operator-parser $splicing-let))
            (list 'splicing-let*
                  (definition-operator-parser parse-splicing-let* 2 #f)
                  (splicing-expression-operator-parser $splicing-let*))
            (list 'splicing-letrec*
                  (definition-operator-parser parse-splicing-letrec* 2 #f)
                  (splicing-expression-operator-parser $splicing-letrec*))
            (list 'splicing-let-values
                  (definition-operator-parser parse-splicing-let-values 2 #f)
                  (splicing-expression-operator-parser $splicing-let-values))
            (list 'splicing-let*-values
                  (definition-operator-parser parse-splicing-let*-values 2 #f)
                  (splicing-expression-operator-parser $splicing-let*-values))
            (list 'splicing-letrec*-values
                  (definition-operator-parser parse-splicing-letrec*-values 2 #f)
                  (splicing-expression-operator-parser $splicing-letrec*-values)))))
    (for-each (lambda (id) (env-bind! env.scope id vocab.expression-auxiliary (syntax-peek id)))
              b*.expr-aux)
    (for-each (lambda (id op) (env-bind! env.scope id vocab.definition-operator op))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id op.def op.expr) (env-bind! env.scope id
                                                     vocab.definition-operator op.def
                                                     vocab.expression-operator op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id op) (env-bind! env.scope id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (env-extend env.empty env.scope)))