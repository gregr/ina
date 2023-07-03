;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-quote        env stx)         ($quote (syntax->datum stx)))
(define (parse-quote-syntax env stx)         ($quote stx))
(define (parse-if           env e.c e.t e.f) ($if (parse-expression env e.c)
                                                  (parse-expression env e.t)
                                                  (parse-expression env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (parse-case-lambda env . e*.cc)
  (define (parse-case-lambda-clause e.cc)
    (let ((e* (syntax->list e.cc)))
      (when (or (null? e*) (null? (cdr e*))) (error "not a case-lambda clause" e.cc))
      (cons (syntax->improper-list (car e*)) (lambda (env) (parse-body env (cdr e*))))))
  (apply $case-lambda/env env (map parse-case-lambda-clause e*.cc)))

(define (parse-and    env . e*) (apply $and    (parse-expression* env e*)))
(define (parse-or     env . e*) (apply $or     (parse-expression* env e*)))
(define (parse-when   env . e*) (apply $when   (parse-expression* env e*)))
(define (parse-unless env . e*) (apply $unless (parse-expression* env e*)))

(define (parse-cond env clause . clause*)
  (let loop ((c* (cons clause clause*)))
    (cond ((null? c*) $void)
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (error "empty clause" c))
                  (let ((e.test (car e*)) (e* (cdr e*)))
                    (cond ((expression-auxiliary? 'else env e.test)
                           (when   (null? e*) (error "empty else clause" c))
                           (unless (null? c*) (error "else clause is not last" c))
                           (parse-body env e*))
                          ((null? e*)
                           ($let '(test) (list (parse-expression env e.test))
                                 (lambda ($test) ($if $test $test (loop c*)))))
                          ((expression-auxiliary? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (error "=> is not followed by one procedure" c))
                           ($let '(test) (list (parse-expression env e.test))
                                 (lambda ($test)
                                   ($if $test
                                        ($call (parse-expression env (cadr e*)) $test)
                                        (loop c*)))))
                          (else ($if (parse-expression env e.test)
                                     (parse-body env e*)
                                     (loop c*))))))))))

(define ((parse-case/$= $=) env e clause . clause*)
  ($let
    '(x) (list (parse-expression env e))
    (lambda ($x)
      (let loop ((c* (cons clause clause*)))
        (cond
          ((null? c*) $void)
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (error "empty clause" c))
                  (let ((e.data (car e*)) (e* (cdr e*)))
                    (cond
                      ((null? e*) (error "empty clause body" c))
                      ((expression-auxiliary? 'else env e.data)
                       (when   (null? e*) (error "empty else clause" c))
                       (unless (null? c*) (error "else clause is not last" c))
                       (parse-body env e*))
                      ((expression-auxiliary? '=> env e.data)
                       (unless (null? (cdr e*)) (error "=> is not followed by one procedure" c))
                       (unless (null? clause*) (error "=> clause is not last" c))
                       ($call (parse-expression env (car e*)) $x))
                      (else ($if (apply $or (map (lambda (d) ($= $x ($quote d)))
                                                 (syntax->list e.data)))
                                 (parse-body env e*)
                                 (loop c*))))))))))))

(define parse-caseq (parse-case/$= $eq?))
(define parse-casev (parse-case/$= $eqv?))

(define (parse-assert env . stx*.test)
  (apply $begin (map (lambda (stx.test)
                       ($unless (parse-expression env stx.test)
                                ($panic ($quote 'assert) ($quote stx.test))))
                     stx*.test)))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pair* e1)) (param* (map car bpair*)))
        (parse-param* param*)
        (apply $call ($letrec/env env (list e0)
                                  (lambda (env)
                                    (values (list ($lambda/env env param* (lambda (env)
                                                                            (parse-body env e*))))
                                            (parse-expression env e0))))
               (parse-expression* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pair* e0)) (param* (map car bpair*)))
        (parse-param* param*)
        ($let/env env param* (parse-expression* env (map cdr bpair*))
                  (lambda (env) (parse-body env (cons e1 e*)))))))

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
    (etc-splicing-expression-operator-parser
      $splicing (lambda (stx*) (lambda (dst scope env)
                                 (defstate-add-expression dst (lambda () (parse-body env stx*)))))))

  (define (splicing-expression-operator-parser $splicing)
    (etc-splicing-expression-operator-parser
      $splicing (lambda (stx*) (lambda (dst scope env)
                                 (defstate-add-expression dst (lambda () (apply parse-begin-expression env stx*))))))))

(define (parse-quasiquote env stx.qq)
  (define (finish quote? x) (if quote? (parse-quote env x) x))
  (define (operand qq) (car (syntax-unwrap (cdr qq))))
  (define (operation? qq tag)
    (and (pair? qq)
         (let ((qq.d (syntax-unwrap (cdr qq))))
           (and (pair? qq.d)
                (let ((qq.dd (syntax-unwrap (cdr qq.d))))
                  (and (null? qq.dd)
                       (identifier? (car qq))
                       (eq? (env-ref^ env (car qq) vocab.quasiquote) tag)))))))
  (define (tag tag-value P) ($list ($quote tag-value) P))
  (let-values
    (((quote? e)
      (let loop ((stx.qq stx.qq) (level 0))
        (let ((qq (syntax-unwrap stx.qq)))
          (cond ((and (= level 0) (pair? qq)
                      (let ((qq.a (syntax-unwrap (car qq))))
                        (operation? qq.a 'unquote-splicing)))
                 (let ((rand (operand (syntax-unwrap (car qq)))))
                   (let-values (((quote? rest) (loop (cdr qq) level)))
                     (let ((rest (finish quote? rest)))
                       (values #f ($append (parse-expression env rand) rest))))))
                ((and (< 0 level) (operation? qq 'unquote-splicing))
                 (let ((rand (operand qq)))
                   (let-values (((quote? e) (loop rand (- level 1))))
                     (if quote?
                         (values #t stx.qq)
                         (values #f (tag 'unquote-splicing e))))))
                ((operation? qq 'unquote)
                 (let ((rand (operand qq)))
                   (if (= level 0)
                       (values #f (parse-expression env rand))
                       (let-values (((quote? e) (loop rand (- level 1))))
                         (if quote?
                             (values #t stx.qq)
                             (values #f (tag 'unquote e)))))))
                ((operation? qq 'quasiquote)
                 (let ((rand (operand qq)))
                   (let-values (((quote? e) (loop rand (+ level 1))))
                     (if quote?
                         (values #t stx.qq)
                         (values #f (tag 'quasiquote e))))))
                ((pair? qq)
                 (let-values (((quote.a? a) (loop (car qq) level))
                              ((quote.b? b) (loop (cdr qq) level)))
                   (if (and quote.a? quote.b?)
                       (values #t stx.qq)
                       (values #f ($cons (finish quote.a? a) (finish quote.b? b))))))
                ((vector? qq)
                 (let-values (((quote? e) (loop (vector->list qq) level)))
                   (if quote?
                       (values #t stx.qq)
                       (values #f ($pcall 'apply ($prim 'vector) e)))))
                (else (when (and (identifier? stx.qq) (env-ref^ env stx.qq vocab.quasiquote))
                        (error "misplaced quasiquote operator" stx.qq))
                      (values #t stx.qq)))))))
    (finish quote? e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-body env stx.body)
  (let ((stx* (syntax->list stx.body)))
    (when (null? stx*) (error "no expression" stx.body))
    (define (^def dst scope env)
      (let ((dst (apply parse-begin-definition dst scope env stx*)))
        (unless (defstate-expression dst) (error "no expression after definitions" stx.body))
        dst))
    ($provenance/syntax ($body env ^def) stx.body)))

(define (parse-begin-definition dst env.scope env . stx*)
  (foldl (lambda (stx dst) (parse-definition dst env.scope env stx)) dst stx*))

(define (parse-introduce dst env.scope env . stx*) (env-introduce*! env.scope stx*) dst)

(define (parse-introduce-alias dst env.scope env id.lhs id.rhs)
  (parse-undefined-identifier env.scope id.lhs)
  (parse-identifier id.rhs)
  (let ((v=>v (env-ref env id.rhs)))
    (unless v=>v (error "unbound identifier" id.rhs))
    (env-set! env.scope id.lhs v=>v))
  dst)

(define (parse-define dst env.scope env lhs . stx*.rhs)
  (cond
    ((identifier? lhs)
     (unless (and (pair? stx*.rhs) (null? (cdr stx*.rhs)))
       (error "not a single expression" stx*.rhs))
     ($define dst env.scope lhs (lambda () (parse-expression env (car stx*.rhs)))))
    (else (let loop ((lhs lhs) (^rhs (lambda (env) (parse-body env stx*.rhs))))
            (let ((x (syntax-unwrap lhs)))
              (cond ((pair?       x) (let ((p*~ (syntax->improper-list (cdr x))))
                                       (loop (car x) (lambda (env) ($lambda/env env p*~ ^rhs)))))
                    ((identifier? x) ($define dst env.scope x (lambda () (^rhs env))))
                    (else            (error "not a definable form" lhs))))))))

(define (parse-define-values dst env.scope env stx.lhs*~ e.rhs)
  (let* ((lhs*~   (syntax->improper-list stx.lhs*~))
         (lhs*    (improper-list->list lhs*~))
         (v*.^rhs (lambda () ($call-with-values ($thunk (parse-expression env e.rhs))
                                                ($lambda lhs*~ $vector))))
         (v*.addr (fresh-address 'vec.value*))
         ($v*     ($ref v*.addr)))
    (foldl (lambda (i lhs dst)
             ($define dst env.scope lhs (lambda () ($vector-ref $v* ($quote i)))))
           (defstate-define dst v*.addr v*.^rhs)
           (iota (length lhs*))
           lhs*)))

(define ($splicing-rec dst env.scope env ^def ^body)
  (let* ((env.scope.inner (make-env))
         (env             (env-compose env env.scope.inner))
         (dst             (^def dst env.scope.inner env)))
    (^body dst env.scope env)))

(define ($splicing-nonrec dst env.scope env ^def ^body)
  (let ((env.scope.inner (make-env)))
    (^body (^def dst env.scope.inner env) env.scope (env-compose env env.scope.inner))))

(define ($splicing-local dst env.scope env stx.def* ^body)
  (let ((def* (syntax->list stx.def*)))
    ($splicing-rec dst env.scope env
                   (lambda (dst scope env) (apply parse-begin-definition dst scope env def*))
                   ^body)))

(splicing-local
  ((define ($splicing $splicing-etc parse-def dst env.scope env stx.bpair* ^body)
     (let ((bpair* (parse-binding-pair* stx.bpair*)))
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
     (let loop ((stx*.bpair* (syntax->list stx.bpair*))
                (dst         dst)
                (scope       env.scope)
                (env         env))
       (cond ((null? stx*.bpair*) (^body dst scope env))
             (else ($splicing-etc dst scope env (list (car stx*.bpair*))
                                  (lambda (dst scope env)
                                    (loop (cdr stx*.bpair*) dst scope env))))))))
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

(define env.minimal
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
            (cons 'caseq          (expression-operator-parser parse-caseq        2 #f))
            (cons 'casev          (expression-operator-parser parse-casev        2 #f))
            (cons 'assert         (expression-operator-parser parse-assert       1 #f))
            (cons 'case-lambda    (expression-operator-parser parse-case-lambda  0 #f))
            (cons 'lambda         (expression-operator-parser parse-lambda       2 #f))
            (cons 'local          (nonsplicing-expression-operator-parser $splicing-local))
            (cons 'let*           (nonsplicing-expression-operator-parser $splicing-let*))
            (cons 'letrec*        (nonsplicing-expression-operator-parser $splicing-letrec*))
            (cons 'let-values     (nonsplicing-expression-operator-parser $splicing-let-values))
            (cons 'let*-values    (nonsplicing-expression-operator-parser $splicing-let*-values))
            (cons 'letrec*-values (nonsplicing-expression-operator-parser $splicing-letrec*-values))
            (cons 'let            (expression-operator-parser parse-let          2 #f))))
        (b*.qq '(unquote unquote-splicing))
        (b*.qq-and-expr (list (cons 'quasiquote (expression-operator-parser parse-quasiquote 1 1))))
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
    (for-each (lambda (id) (env-bind! env.scope id vocab.quasiquote (syntax-peek id)))
              b*.qq)
    (for-each (lambda (id op) (env-bind! env.scope id
                                         vocab.expression-operator op
                                         vocab.quasiquote          (syntax-peek id)))
              (map car b*.qq-and-expr) (map cdr b*.qq-and-expr))
    (for-each (lambda (id op) (env-bind! env.scope id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (env-freeze! env.scope)
    env.scope))
