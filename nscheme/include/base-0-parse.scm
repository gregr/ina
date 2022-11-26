(define (parse-identifier id) (unless (identifier? id) (raise-syntax-error "not an identifier" id)))

(define (parse-binding-pairs e.bpairs)
  (define (parse-binding-pair e.bpair)
    (let ((e* (syntax->list e.bpair)))
      (unless (= (length e*) 2) (raise-syntax-error "binding pair without 2 elements" e.bpair))
      (parse-identifier (car e*))
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
      ($case-lambda-clause env (syntax->improper-list (car e*))
                           (lambda (env . _) (parse-body env (cdr e*))))))
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
                           ($let env '(test) (list (parse env e.test))
                                 (lambda (_ addr.test)
                                   ($if ($ref addr.test) ($ref addr.test) (loop c*)))))
                          ((expression-keyword? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (raise-syntax-error "=> is not followed by one procedure" c))
                           ($let env '(test) (list (parse env e.test))
                                 (lambda (_ addr.test)
                                   ($if ($ref addr.test)
                                        ($call (parse env (cadr e*)) ($ref addr.test))
                                        (loop c*)))))
                          (else ($if (parse env e.test) (parse-body env e*) (loop c*))))))))))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pairs e1)) (param* (map car bpair*)))
        ($call ($letrec env (list e0)
                        (lambda (env addr)
                          (values (list ($lambda env param* (lambda (env . _) (parse-body env e*))))
                                  ($ref addr))))
               (parse* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pairs e0)) (param* (map car bpair*)))
        ($let env param* (parse* env (map cdr bpair*))
              (lambda (env . _) (parse-body env (cons e1 e*)))))))

(define (parse-begin-expression env e . e*)
  (let loop ((e e) (e* e*))
    (cond ((null? e*) (parse env e))
          (else       ($begin (parse env e) (loop (car e*) (cdr e*)))))))

(define (splicing->expression-operator-parser ^%id.splicing)
  (expression-operator-parser
    (lambda (env stx.def* . stx*)
      (parse env `(,%let () (,(^%id.splicing) ,stx.def* (,%let () . ,stx*)))))
    2 #f))

(define ((definition->expression-operator-parser ^%id.def) env . stx*)
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

(define (parse-introduce-alias dst env.scope env id.lhs id.rhs)
  (parse-identifier id.lhs)
  (parse-identifier id.rhs)
  (let ((addr (env-address env id.rhs)))
    (unless addr (raise-syntax-error "unbound identifier" id.rhs))
    (env-bind! env.scope id.lhs addr)))

(define (parse-define dst env.scope env lhs . stx*.rhs)
  (cond
    ((identifier? lhs) (unless (and (pair? stx*.rhs) (null? (cdr stx*.rhs)))
                         (raise-syntax-error "not a single expression" stx*.rhs))
                       ($define dst env.scope lhs (lambda () (parse env (car stx*.rhs)))))
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
                    ($pcall call-with-values (ast:lambda #f '() (parse env e.rhs))
                            ($lambda env lhs*~ (lambda (env . addr*)
                                                 (apply $pcall vector (map $ref addr*)))))))
         (v*.addr (fresh-address 'vec.value*))
         ($v*     ($ref v*.addr)))
    (foldl (lambda (i lhs dst)
             ($define dst env.scope lhs (lambda () ($pcall vector-ref $v* ($quote i)))))
           (defstate-define dst v*.addr v*.^rhs)
           (iota (length lhs*))
           lhs*)))

(define (parse-splicing-local dst env.scope env stx.def* . stx*)
  (let* ((stx*.local-body (syntax->list stx.def*))
         (env.scope.inner (make-env))
         (env             (env-extend env env.scope.inner))
         (dst             (apply parse-begin-definition dst env.scope.inner env stx*.local-body)))
    (apply parse-begin-definition dst env.scope env stx*)))

(splicing-local
  ((define (etc parse-def dst env.scope env stx.bpair* . stx*)
     (let ((bpair* (parse-binding-pairs stx.bpair*)))
       (let* ((env.scope.inner (make-env))
              (dst (foldl (lambda (lhs rhs dst) (parse-def dst env.scope.inner env lhs rhs))
                          dst (map car bpair*) (map cdr bpair*))))
         (apply parse-begin-definition dst env.scope (env-extend env env.scope.inner) stx*)))))
  (define (parse-splicing-let        . a*) (apply etc parse-define        a*))
  (define (parse-splicing-let-values . a*) (apply etc parse-define-values a*)))

(splicing-local
  ((define (etc %splicing-let-etc dst env.scope env stx.bpair* . stx*)
     (parse-definition
       dst env.scope env
       (let loop ((bpair* (parse-binding-pairs stx.bpair*)))
         (cond ((null? bpair*) `(,%splicing-let-etc ()              . ,stx*))
               (else           `(,%splicing-let-etc (,(car bpair*)) ,(loop (cdr bpair*)))))))))
  (define (parse-splicing-let*        . a*) (apply etc %splicing-let        a*))
  (define (parse-splicing-let*-values . a*) (apply etc %splicing-let-values a*)))

(splicing-local
  ((define (etc %define-etc dst env.scope env stx.bpair* . stx*)
     (let ((bpair* (parse-binding-pairs stx.bpair*)))
       (parse-definition dst env.scope env
                         `(,%splicing-local ,(map (lambda (lhs rhs) `(,%define-etc ,lhs ,rhs))
                                                  (map car bpair*) (map cdr bpair*))
                                            . ,stx*)))))
  (define (parse-splicing-letrec*        . a*) (apply etc %define        a*))
  (define (parse-splicing-letrec*-values . a*) (apply etc %define-values a*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment and qualified identifiers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env.base-0
  (let ((env.scope (make-env))
        (primitive*
          (list
            procedure-metadata procedure-metadata-set!
            eq? eqv? null? procedure? pair? cons car cdr
            string->symbol symbol->string symbol? string? vector vector? vector-length vector-ref
            vector->svector svector->vector svector svector? svector-length svector-ref
            mvector->vector mvector make-mvector mvector? mvector-length mvector-ref mvector-set!
            utf8->string string->utf8 bytevector bytevector? bytevector-length bytevector-u8-ref
            mbytevector->bytevector make-mbytevector mbytevector mbytevector?
            mbytevector-length mbytevector-u8-ref mbytevector-u8-set!
            number? exact? integer? inexact? = <= < + - * / quotient remainder truncate
            integer-length bitwise-arithmetic-shift << >> & \| ^))
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
            (cons 'local          (splicing->expression-operator-parser (lambda () %splicing-local)))
            (cons 'let            (expression-operator-parser parse-let          2 #f))
            (cons 'let*           (splicing->expression-operator-parser (lambda () %splicing-let*)))
            (cons 'letrec*        (splicing->expression-operator-parser (lambda () %splicing-letrec*)))
            (cons 'letrec         (splicing->expression-operator-parser (lambda () %splicing-letrec)))
            (cons 'let-values     (splicing->expression-operator-parser (lambda () %splicing-let-values)))
            (cons 'let*-values    (splicing->expression-operator-parser (lambda () %splicing-let*-values)))
            (cons 'letrec*-values (splicing->expression-operator-parser (lambda () %splicing-letrec*-values)))
            (cons 'letrec-values  (splicing->expression-operator-parser (lambda () %splicing-letrec-values)))))
        (b*.def-and-expr
          (list
            (list 'begin
                  (definition-operator-parser parse-begin-definition 0 #f)
                  (expression-operator-parser parse-begin-expression 1 #f))
            (list 'splicing-local
                  (definition-operator-parser parse-splicing-local 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-local)))
            (list 'splicing-let
                  (definition-operator-parser parse-splicing-let 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-let)))
            (list 'splicing-let*
                  (definition-operator-parser parse-splicing-let* 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-let*)))
            (list 'splicing-letrec*
                  (definition-operator-parser parse-splicing-letrec* 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-letrec*)))
            (list 'splicing-letrec
                  (definition-operator-parser parse-splicing-letrec* 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-letrec)))
            (list 'splicing-let-values
                  (definition-operator-parser parse-splicing-let-values 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-let-values)))
            (list 'splicing-let*-values
                  (definition-operator-parser parse-splicing-let*-values 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-let*-values)))
            (list 'splicing-letrec*-values
                  (definition-operator-parser parse-splicing-letrec*-values 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-letrec*-values)))
            (list 'splicing-letrec-values
                  (definition-operator-parser parse-splicing-letrec*-values 2 #f)
                  (definition->expression-operator-parser (lambda () %splicing-letrec-values))))))
    (for-each (lambda (id)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.expression-keyword addr (syntax-peek id))))
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
                  (env-set!  env.scope vocab.definition-operator addr op)))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id op.def op.expr)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.definition-operator addr op.def)
                  (env-set!  env.scope vocab.expression-operator addr op.expr)))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id op)
                (let ((addr (identifier->fresh-address id)))
                  (env-bind! env.scope id addr)
                  (env-set!  env.scope vocab.expression-operator addr op)))
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
