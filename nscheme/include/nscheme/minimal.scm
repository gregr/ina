;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-body env stx.body)
  (let ((stx* (syntax->list stx.body)))
    (define (^def env.d env) ($d:source (parse-begin-definition* env.d env stx*) stx.body))
    ($source ($body env ^def) stx.body)))

(define (parse-begin-definition* env.d env stx*)
  (apply $d:begin (map (lambda (stx) (parse-definition env.d env stx)) stx*)))
(define (parse-begin-definition env.d env . stx*) (parse-begin-definition* env.d env stx*))

(define (parse-introduce env.d env . stx*) (env-introduce*! env.d stx*) ($d:begin))

(define (parse-define-alias env.d env id.lhs id.rhs)
  (parse-undefined-identifier env.d id.lhs)
  (parse-identifier id.rhs)
  (let ((v=>v (env-ref env id.rhs)))
    (unless v=>v (raise-unbound-identifier-parse-error "unbound identifier" id.rhs #f env))
    (env-set! env.d id.lhs v=>v))
  ($d:begin))

(define (parse-operator-binding finish stx.lhs stx*.rhs)
  (if (identifier? stx.lhs)
      (if (and (pair? stx*.rhs) (null? (cdr stx*.rhs)))
          (finish stx.lhs (lambda (env) (parse-expression env (car stx*.rhs))))
          (raise-parse-error "not a single expression" stx*.rhs))
      (let loop ((stx.lhs stx.lhs) (^rhs (lambda (env) (parse-body env stx*.rhs))))
        (if (identifier? stx.lhs)
            (finish stx.lhs ^rhs)
            (let ((x (syntax-unwrap stx.lhs)))
              (unless (pair? x) (raise-parse-error "not a definable form" stx.lhs))
              (let ((p*~ (syntax->improper-list (cdr x))))
                (loop (car x) (lambda (env) ($lambda/env env p*~ ^rhs)))))))))

(define (parse-define env.d env stx.lhs . stx*.rhs)
  (define (finish id.lhs ^rhs) ($d:define env.d id.lhs (lambda () (^rhs env))))
  (parse-operator-binding finish stx.lhs stx*.rhs))

(define (parse-define-values env.d env stx.lhs*~ e.rhs)
  (let* ((lhs*~     (syntax->improper-list stx.lhs*~))
         (lhs*      (improper-list->list lhs*~))
         (id.v*     'vec.value*)
         (env.local (make-env)))
    (apply $d:begin
           ($d:define env.local id.v* (lambda () ($apply/values ($lambda lhs*~ $vector)
                                                                (parse-expression env e.rhs))))
           (map (lambda (i lhs)
                  ($d:define env.d lhs (lambda () ($vector-ref (parse-expression env.local id.v*)
                                                               ($quote i)))))
                (iota (length lhs*))
                lhs*))))

(define (parse-mdefine env.d env id.lhs stx.rhs)
  (parse-identifier id.lhs)
  (let ((env.local (make-env)))
    (env-introduce-boxed! env.d id.lhs (lambda () (parse-expression env.local id.lhs)))
    ($d:define env.local id.lhs (lambda () ($box (parse-expression env stx.rhs))))))

(splicing-local
  ((define ($definer-rec env ^def ^body)
     (let* ((env.d.inner (make-env))
            (env         (env-conjoin env.d.inner env)))
       ($d:begin (^def env.d.inner env) (^body env))))
   (define ($definer-nonrec env ^def ^body)
     (let ((env.d.inner (make-env)))
       ($d:begin (^def env.d.inner env) (^body (env-conjoin env.d.inner env)))))
   (define (($make-binder $definer parse-def) env stx.bpair* ^body)
     (let ((bpair* (parse-binding-pair* stx.bpair*)))
       ($definer env (lambda (env.d env)
                       (apply $d:begin (map (lambda (lhs rhs) (parse-def env.d env lhs rhs))
                                            (map car bpair*) (map cdr bpair*))))
                 ^body)))
   (define (($make-nesting-binder $binder) env stx.bpair* ^body)
     (let loop ((stx*.bpair* (syntax->list stx.bpair*)) (env env))
       (cond ((null? stx*.bpair*) (^body env))
             (else ($binder env (list (car stx*.bpair*))
                            (lambda (env) (loop (cdr stx*.bpair*) env))))))))
  (define ($binder-local env stx.def* ^body)
    (let ((def* (syntax->list stx.def*)))
      ($definer-rec env (lambda (env.d env) (parse-begin-definition* env.d env def*)) ^body)))
  (define $binder-let            ($make-binder $definer-nonrec parse-define))
  (define $binder-mlet           ($make-binder $definer-nonrec parse-mdefine))
  (define $binder-let-values     ($make-binder $definer-nonrec parse-define-values))
  (define $binder-letrec*        ($make-binder $definer-rec    parse-define))
  (define $binder-letrec*-values ($make-binder $definer-rec    parse-define-values))
  (define $binder-let*           ($make-nesting-binder $binder-let))
  (define $binder-let*-values    ($make-nesting-binder $binder-let-values)))

(splicing-local
  ((define ((binder-parser $binder) env.d env stx.def* . stx*)
     ($binder env stx.def* (lambda (env) (parse-begin-definition* env.d env stx*)))))
  (define parse-splicing-local          (binder-parser $binder-local))
  (define parse-splicing-let            (binder-parser $binder-let))
  (define parse-splicing-mlet           (binder-parser $binder-mlet))
  (define parse-splicing-let*           (binder-parser $binder-let*))
  (define parse-splicing-letrec*        (binder-parser $binder-letrec*))
  (define parse-splicing-let-values     (binder-parser $binder-let-values))
  (define parse-splicing-let*-values    (binder-parser $binder-let*-values))
  (define parse-splicing-letrec*-values (binder-parser $binder-letrec*-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-quote        env stx)         ($quote (syntax->datum stx)))
(define (parse-quote-syntax env stx)         ($quote stx))
(define (parse-if           env e.c e.t e.f) ($if (parse-expression env e.c)
                                                  (parse-expression env e.t)
                                                  (parse-expression env e.f)))
(define (parse-lambda       env param . e*)  (parse-case-lambda env (cons param e*)))

(define (parse-case-lambda env . stx*.cc)
  (let* ((cc*        (map (lambda (stx.cc)
                            (let ((stx* (syntax->list stx.cc)))
                              (when (or (null? stx*) (null? (cdr stx*)))
                                (raise-parse-error "not a case-lambda clause" stx.cc))
                              stx*))
                          stx*.cc))
         (param*     (map syntax->improper-list (map car cc*)))
         (env->body* (map (lambda (body) (lambda (env) (parse-body env body))) (map cdr cc*))))
    ($case-lambda/env env param* env->body*)))

(define (parse-apply/values env rator vrand)
  ($apply/values (parse-expression env rator) (parse-expression env vrand)))

(define (parse-case-values env ve . clause*)
  ($apply/values (apply parse-case-lambda env clause*) (parse-expression env ve)))

(define (parse-and    env . e*) (apply $and    (parse-expression* env e*)))
(define (parse-or     env . e*) (apply $or     (parse-expression* env e*)))
(define (parse-when   env . e*) (apply $when   (parse-expression* env e*)))
(define (parse-unless env . e*) (apply $unless (parse-expression* env e*)))

(define (parse-cond env clause . clause*)
  (let loop ((c* (cons clause clause*)))
    (cond ((null? c*) ($pcall values))
          (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                  (when (null? e*) (raise-parse-error "empty clause" c))
                  (let ((e.test (car e*)) (e* (cdr e*)))
                    (cond ((expression-auxiliary? 'else env e.test)
                           (when   (null? e*) (raise-parse-error "empty else clause" c))
                           (unless (null? c*) (raise-parse-error "else clause is not last" c))
                           (parse-body env e*))
                          ((null? e*)
                           ($let1 'test (parse-expression env e.test)
                                  (lambda ($test) ($if $test $test (loop c*)))))
                          ((expression-auxiliary? '=> env (car e*))
                           (unless (and (pair? (cdr e*)) (null? (cddr e*)))
                             (raise-parse-error "=> is not followed by one procedure" c))
                           ($let1 'test (parse-expression env e.test)
                                  (lambda ($test)
                                    ($if $test
                                         ($call (parse-expression env (cadr e*)) $test)
                                         (loop c*)))))
                          (else ($if (parse-expression env e.test)
                                     (parse-body env e*)
                                     (loop c*))))))))))

(define (parse-case env e clause . clause*)
  ($let1 'x (parse-expression env e)
         (lambda ($x)
           (let loop ((c* (cons clause clause*)))
             (cond
               ((null? c*) ($pcall values))
               (else (let* ((c (car c*)) (c* (cdr c*)) (e* (syntax->list c)))
                       (when (null? e*) (raise-parse-error "empty clause" c))
                       (let ((e.data (car e*)) (e* (cdr e*)))
                         (cond
                           ((null? e*) (raise-parse-error "empty clause body" c))
                           ((expression-auxiliary? 'else env e.data)
                            (when   (null? e*) (raise-parse-error "empty else clause" c))
                            (unless (null? c*) (raise-parse-error "else clause is not last" c))
                            (parse-body env e*))
                           ((expression-auxiliary? '=> env e.data)
                            (unless (null? (cdr e*))
                              (raise-parse-error "=> is not followed by one procedure" c))
                            (unless (null? c*) (raise-parse-error "=> clause is not last" c))
                            ($call (parse-expression env (car e*)) $x))
                           (else ($if (apply $or (map (lambda (d) ($literal-equal? d $x))
                                                      (syntax->datum e.data)))
                                      (parse-body env e*)
                                      (loop c*))))))))))))

(define (parse-assert env . stx*.test)
  (apply $begin (map (lambda (stx.test)
                       ($unless (parse-expression env stx.test)
                                ($error ($quote "failed assertion") ($quote stx.test))))
                     stx*.test)))

(define (parse-let env e0 e1 . e*)
  (if (identifier? e0)
      (let* ((bpair* (parse-binding-pair* e1)) (param* (map car bpair*)))
        ($call* ($letrec/env env (list e0)
                             (lambda (env)
                               (values (list ($lambda/env env param* (lambda (env)
                                                                       (parse-body env e*))))
                                       (parse-expression env e0))))
                (parse-expression* env (map cdr bpair*))))
      (let* ((bpair* (parse-binding-pair* e0)) (param* (map car bpair*)))
        ($let/env env param* (parse-expression* env (map cdr bpair*))
                  (lambda (env) (parse-body env (cons e1 e*)))))))

(define (parse-mlet env stx.bpair* . stx*.body)
  (let* ((bpair* (parse-binding-pair* stx.bpair*))
         (param* (map car bpair*)))
    ($let/env env param* (map $box (parse-expression* env (map cdr bpair*)))
              (lambda (env.unboxed)
                (let ((env.boxed (make-env)))
                  (for-each (lambda (id)
                              (let ((E.boxed (parse-expression env.unboxed id)))
                                (env-introduce-boxed! env.boxed id (lambda () E.boxed))))
                            param*)
                  (parse-body (env-conjoin (env-read-only env.boxed) env.unboxed) stx*.body))))))

(define (parse-begin-expression env e . e*)
  (let loop ((e e) (e* e*))
    (cond ((null? e*) (parse-expression env e))
          (else       ($begin (parse-expression env e) (loop (car e*) (cdr e*)))))))

(define ((parse-quasiquote-X vocab tag.enter tag.escape tag.escape-splicing parse-quote-X) env stx)
  (define (finish  result stx.original) (or result (parse-quote-X env stx.original)))
  (define (tag     tag-value e)         (and e ($list ($quote tag-value) e)))
  (define (keyword stx)                 (and (identifier? stx) (env-ref^ env stx vocab)))
  (define (operand qq)                  (car (syntax-unwrap (cdr qq))))
  (define (operation? qq tag)
    (and (eq? (keyword (car qq)) tag) (let ((qq.d (syntax-unwrap (cdr qq))))
                                        (and (pair? qq.d) (null? (syntax-unwrap (cdr qq.d)))))))
  (finish
    (let loop ((stx.qq stx) (level 0))
      (let ((qq (syntax-unwrap stx.qq)))
        (cond ((pair? qq)
               (cond
                 ((operation? qq tag.enter)  (tag tag.enter (loop (operand qq) (+ level 1))))
                 ((operation? qq tag.escape) (if (= level 0)
                                                 (parse-expression env (operand qq))
                                                 (tag tag.escape (loop (operand qq) (- level 1)))))
                 ((and (= level 0) (let ((qq.a (syntax-unwrap (car qq))))
                                     (and (pair? qq.a) (operation? qq.a tag.escape-splicing))))
                  ($append (parse-expression env (operand (syntax-unwrap (car qq))))
                           (let ((stx.qq.d (cdr qq))) (finish (loop stx.qq.d level) stx.qq.d))))
                 ((and (< 0 level) (operation? qq tag.escape-splicing))
                  (tag tag.escape-splicing (loop (operand qq) (- level 1))))
                 (else (let ((a (loop (car qq) level)) (b (loop (cdr qq) level)))
                         (and (or a b) ($cons (finish a (car qq)) (finish b (cdr qq))))))))
              ((vector? qq)     (let ((e (loop (vector->list qq) level)))
                                  (and e ($pcall apply ($quote vector) e))))
              ((keyword stx.qq) (raise-parse-error "misplaced keyword" stx.qq))
              (else             #f))))
    stx))

(define parse-quasiquote
  (parse-quasiquote-X vocab.quasiquote 'quasiquote 'unquote 'unquote-splicing parse-quote))
(define parse-quasiquote-syntax
  (parse-quasiquote-X vocab.quasiquote-syntax 'quasiquote-syntax 'unsyntax 'unsyntax-splicing
                      parse-quote-syntax))

(splicing-local
  ((define ((binder-parser $binder) env stx.def* . stx*)
     (D->E ($binder env stx.def* (lambda (env) ($d:expression
                                                 (lambda () (parse-body env stx*))))))))
  (define parse-local          (binder-parser $binder-local))
  (define parse-let*           (binder-parser $binder-let*))
  (define parse-letrec*        (binder-parser $binder-letrec*))
  (define parse-let-values     (binder-parser $binder-let-values))
  (define parse-let*-values    (binder-parser $binder-let*-values))
  (define parse-letrec*-values (binder-parser $binder-letrec*-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pre-base language syntax environment ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define env.minimal
  (let ((env (make-env))
        (b*.expr-aux '(=> else))
        (b*.def
          (list
            (cons 'define                  (definition-operator-parser parse-define           2 #f))
            (cons 'mdefine                 (definition-operator-parser parse-mdefine          2 2))
            (cons 'define-values           (definition-operator-parser parse-define-values    2 #f))
            (cons 'define-alias            (definition-operator-parser parse-define-alias     2 2))
            (cons 'introduce               (definition-operator-parser parse-introduce        0 #f))
            (cons 'splicing-local          (definition-operator-parser parse-splicing-local   2 #f))
            (cons 'splicing-let            (definition-operator-parser parse-splicing-let     2 #f))
            (cons 'splicing-mlet           (definition-operator-parser parse-splicing-mlet    2 #f))
            (cons 'splicing-let*           (definition-operator-parser parse-splicing-let*    2 #f))
            (cons 'splicing-letrec*        (definition-operator-parser parse-splicing-letrec* 2 #f))
            (cons 'splicing-let-values     (definition-operator-parser parse-splicing-let-values     2 #f))
            (cons 'splicing-let*-values    (definition-operator-parser parse-splicing-let*-values    2 #f))
            (cons 'splicing-letrec*-values (definition-operator-parser parse-splicing-letrec*-values 2 #f))))
        (b*.expr
          (list
            (cons 'quote          (expression-operator-parser parse-quote          1 1))
            (cons 'quote-syntax   (expression-operator-parser parse-quote-syntax   1 1))
            (cons 'if             (expression-operator-parser parse-if             3 3))
            (cons 'and            (expression-operator-parser parse-and            0 #f))
            (cons 'or             (expression-operator-parser parse-or             0 #f))
            (cons 'when           (expression-operator-parser parse-when           2 #f))
            (cons 'unless         (expression-operator-parser parse-unless         2 #f))
            (cons 'cond           (expression-operator-parser parse-cond           1 #f))
            (cons 'case           (expression-operator-parser parse-case           2 #f))
            (cons 'assert         (expression-operator-parser parse-assert         1 #f))
            (cons 'apply/values   (expression-operator-parser parse-apply/values   2 2))
            (cons 'case-lambda    (expression-operator-parser parse-case-lambda    0 #f))
            (cons 'lambda         (expression-operator-parser parse-lambda         2 #f))
            (cons 'case-values    (expression-operator-parser parse-case-values    1 #f))
            (cons 'local          (expression-operator-parser parse-local          2 #f))
            (cons 'let*           (expression-operator-parser parse-let*           2 #f))
            (cons 'letrec*        (expression-operator-parser parse-letrec*        2 #f))
            (cons 'let-values     (expression-operator-parser parse-let-values     2 #f))
            (cons 'let*-values    (expression-operator-parser parse-let*-values    2 #f))
            (cons 'letrec*-values (expression-operator-parser parse-letrec*-values 2 #f))
            (cons 'let            (expression-operator-parser parse-let  2 #f))
            (cons 'mlet           (expression-operator-parser parse-mlet 2 #f))
            (cons 'set!           (expression-operator-parser parse-set! 2 2))))
        (b*.qq '(unquote unquote-splicing))
        (b*.qq-and-expr (list (cons 'quasiquote (expression-operator-parser parse-quasiquote 1 1))))
        (b*.qqs '(unsyntax unsyntax-splicing))
        (b*.qqs-and-expr
          (list (cons 'quasiquote-syntax (expression-operator-parser parse-quasiquote-syntax 1 1))))
        (b*.def-and-expr
          (list
            (list 'expression
                  (definition-operator-parser parse-definition-expression 1 1)
                  (expression-operator-parser parse-expression            1 1))
            (list 'begin
                  (definition-operator-parser parse-begin-definition 0 #f)
                  (expression-operator-parser parse-begin-expression 1 #f)))))
    (for-each (lambda (id) (env-bind! env id vocab.expression-auxiliary (syntax->datum id)))
              b*.expr-aux)
    (for-each (lambda (id op) (env-bind! env id vocab.definition-operator op))
              (map car b*.def) (map cdr b*.def))
    (for-each (lambda (id op.def op.expr) (env-bind! env id
                                                     vocab.definition-operator op.def
                                                     vocab.expression-operator op.expr))
              (map car b*.def-and-expr) (map cadr b*.def-and-expr) (map caddr b*.def-and-expr))
    (for-each (lambda (id) (env-bind! env id vocab.quasiquote (syntax->datum id)))
              b*.qq)
    (for-each (lambda (id op) (env-bind! env id
                                         vocab.expression-operator op
                                         vocab.quasiquote          (syntax->datum id)))
              (map car b*.qq-and-expr) (map cdr b*.qq-and-expr))
    (for-each (lambda (id) (env-bind! env id vocab.quasiquote-syntax (syntax->datum id)))
              b*.qqs)
    (for-each (lambda (id op) (env-bind! env id
                                         vocab.expression-operator op
                                         vocab.quasiquote-syntax   (syntax->datum id)))
              (map car b*.qqs-and-expr) (map cdr b*.qqs-and-expr))
    (for-each (lambda (id op) (env-bind! env id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (env-read-only env)))
