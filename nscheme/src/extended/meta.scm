(define-in-vocabulary current-environment
  vocab.expression
  (operator-parser (lambda (env) ($quote env)) 0 0))

(begin-meta
  (define vocab.syntax-pattern      'syntax-pattern)
  (define vocab.syntax-pattern/...  'syntax-pattern/...)
  (define vocab.syntax-template     'syntax-template)
  (define vocab.syntax-template/... 'syntax-template/...)
  (define vocab.quasisyntax         'quasisyntax)
  (define vocab.quasiquote-syntax   'quasiquote-syntax)
  (define (parse-quote-syntax env stx) ($quote (syntax-prune-level stx (current-mark-level))))
  (define parse-quasiquote-syntax
    (make-parse-quasi vocab.quasiquote-syntax 'quasiquote-syntax 'unsyntax 'unsyntax-splicing #f
                      parse-expression parse-quote-syntax $cons $list->vector $append #f))

  (splicing-local
    ((define (ST:quote        stx)            (vector 'ST:quote        stx))
     (define (ST:unquote      E)              (vector 'ST:unquote      E))
     (define (ST:ref          addr level stx) (vector 'ST:ref          addr level stx))
     (define (ST:cons         a b)            (vector 'ST:cons         a b))
     (define (ST:list->vector t)              (vector 'ST:list->vector t))
     (define (ST:append       a b)            (vector 'ST:append       a b))
     (define (ST:map          t stx)          (vector 'ST:map          t stx))
     (splicing-local
       ((define (ST-tagged? x len tag)
          (and (vector? x) (= (vector-length x) len) (eqv? (vector-ref x 0) tag))))
       (define (ST:quote?        x) (ST-tagged? x 2 'ST:quote))
       (define (ST:unquote?      x) (ST-tagged? x 2 'ST:unquote))
       (define (ST:ref?          x) (ST-tagged? x 4 'ST:ref))
       (define (ST:cons?         x) (ST-tagged? x 3 'ST:cons))
       (define (ST:list->vector? x) (ST-tagged? x 2 'ST:list->vector))
       (define (ST:append?       x) (ST-tagged? x 3 'ST:append))
       (define (ST:map?          x) (ST-tagged? x 3 'ST:map)))
     (define (ST-case  x kf . ?h*) (ST-case* x kf ?h*))
     (define (ST-case* x kf ?h*)   (let ((?h* (pmemp (lambda (?) (? x)) ?h*)))
                                     (if ?h* (apply (cadr ?h*) (vector->list x 1)) (kf x))))
     (define (ST-compile x)
       (let-values
         (((E addr&level=>addr addr=>E)
           (let loop ((x x) (addr&level=>addr '()) (addr=>E '()))
             (define (do-append a b)
               (if (and (ST:map? a) (ST-case b (lambda (b) #f) ST:quote? (lambda (stx) (null? (syntax-unwrap stx)))))
                   (loop a addr&level=>addr addr=>E)
                   (let*-values (((a addr&level=>addr addr=>E) (loop a addr&level=>addr addr=>E))
                                 ((b addr&level=>addr addr=>E) (loop b addr&level=>addr addr=>E)))
                     (values ($append a b) addr&level=>addr addr=>E))))
             (ST-case
               x (lambda (x) (mistake "not an ST" x))
               ST:quote?        (lambda (stx) (values ($source ($quote stx) stx) addr&level=>addr addr=>E))
               ST:unquote?      (lambda (E)   (let ((addr (make-address #f #f)))
                                                (values ($ref addr) addr&level=>addr (cons (cons addr E) addr=>E))))
               ST:ref?          (lambda (addr level stx)
                                  (if (< 0 level)
                                      (let ((a&l (cons addr level)))
                                        (alist-ref/k addr&level=>addr a&l
                                                     (lambda ()
                                                       (let ((addr (make-address (address-name addr) stx)))
                                                         (values ($ref addr) (cons (cons a&l addr) addr&level=>addr) addr=>E)))
                                                     (lambda (addr) (values ($ref addr) addr&level=>addr addr=>E))))
                                      (values ($ref addr) addr&level=>addr addr=>E)))
               ST:cons?         (lambda (a b) (if (ST:map? a)
                                                  (do-append a b)
                                                  (let*-values (((a addr&level=>addr addr=>E) (loop a addr&level=>addr addr=>E))
                                                                ((b addr&level=>addr addr=>E) (loop b addr&level=>addr addr=>E)))
                                                    (values ($cons a b) addr&level=>addr addr=>E))))
               ST:list->vector? (lambda (x)   (let-values (((x addr&level=>addr addr=>E) (loop x addr&level=>addr addr=>E)))
                                                (values ($list->vector x) addr&level=>addr addr=>E)))
               ST:append?       do-append
               ST:map?          (lambda (x stx)
                                  (let-values (((E a&l=>a addr=>E) (loop x '() addr=>E)))
                                    (when (null? a&l=>a) (raise-parse-error "misplaced ellipsis" stx))
                                    (let ((param* (map cdr a&l=>a)))
                                      (let loop ((a&l* (reverse (map car a&l=>a))) (a&l=>a addr&level=>addr) (arg* '()))
                                        (if (null? a&l*)
                                            (let ((E ($call* ($quote map) (cons (E:lambda param* E) arg*))))
                                              (values (if (ST:map? x) ($call ($quote append*) E) E) a&l=>a addr=>E))
                                            (let* ((a&l (car a&l*)) (a&l* (cdr a&l*)) (addr (car a&l)) (level (- (cdr a&l) 1)))
                                              (if (< 0 level)
                                                  (let ((a&l (cons addr level))
                                                        (addr (make-address (address-name addr) (address-note addr))))
                                                    (loop a&l* (cons (cons a&l addr) a&l=>a) (cons ($ref addr) arg*)))
                                                  (loop a&l* a&l=>a (cons ($ref addr) arg*)))))))))))))
         (unless (null? addr&level=>addr) (raise-parse-error "not enough ellipses" (address-note (cdar addr&level=>addr))))
         (let ((addr=>E (reverse addr=>E)))
           (if (null? addr=>E) E (E:let (map car addr=>E) (map cdr addr=>E) E))))))
    (define (parse-syntax-template env stx)
      (let loop ((stx stx))
        (let ((a&l (and (identifier? stx) (env-vocabulary-ref env stx vocab.syntax-template))))
          (if a&l
              (let ((addr (car a&l)) (level (cdr a&l))) (ST:ref addr level stx))
              (let ((x (syntax-unwrap stx)))
                (cond ((pair? x)   (let ((a (loop (car x))) (b (loop (cdr x))))
                                     (if (and (ST:quote? a) (ST:quote? b)) (ST:quote stx) (ST:cons a b))))
                      ((vector? x) (let ((x (loop (vector->list x))))
                                     (if (ST:quote? x) (ST:quote stx) (ST:list->vector x))))
                      (else        (ST:quote stx))))))))
    (define parse-syntax-template/...
      (let ((parse-ST (make-parse-quasi vocab.syntax-template/... #f #f #f '...
                                        #f parse-syntax-template ST:cons ST:list->vector #f ST:map)))
        (lambda (env stx) (ST-compile (parse-ST env (syntax-prune-level stx (current-mark-level)))))))
    (define parse-quasisyntax
      (let ((parse-ST (make-parse-quasi vocab.quasisyntax 'quasisyntax 'unsyntax 'unsyntax-splicing '...
                                        (lambda (env stx) (ST:unquote (parse-expression env stx)))
                                        parse-syntax-template ST:cons ST:list->vector ST:append ST:map)))
        (lambda (env stx) (ST-compile (parse-ST env (syntax-prune-level stx (current-mark-level)))))))))

(define-in-vocabulary quote-syntax vocab.expression (operator-parser parse-quote-syntax        1 1))
(define-in-vocabulary syntax       vocab.expression (operator-parser parse-syntax-template/... 1 1))

(add-in-vocabulary _ vocab.syntax-pattern '_)
(add-in-vocabulary ...
  vocab.syntax-pattern/...  '...
  vocab.syntax-template/... '...
  vocab.quasisyntax         '...)
(define-in-vocabulary unsyntax
  vocab.quasiquote-syntax 'unsyntax
  vocab.quasisyntax       'unsyntax)
(define-in-vocabulary unsyntax-splicing
  vocab.quasiquote-syntax 'unsyntax-splicing
  vocab.quasisyntax       'unsyntax-splicing)
(define-in-vocabulary quasiquote-syntax
  vocab.quasiquote-syntax 'quasiquote-syntax
  vocab.expression        (operator-parser parse-quasiquote-syntax 1 1))
(define-in-vocabulary quasisyntax
  vocab.quasisyntax 'quasisyntax
  vocab.expression  (operator-parser parse-quasisyntax 1 1))

(begin-meta
  (define ((make-macro-parser introduce-definitions? parse) env.op op)
    (let ((transcribe (syntax-transcribe/parse introduce-definitions? parse)))
      (lambda (env.use stx) (transcribe stx op env.op env.use))))
  (define (dismantle-operator-binding stx)
    (apply (lambda (_ lhs . rhs*)
             (unless (pair? rhs*)
               (raise-parse-error "missing right-hand-side expression" stx))
             (when (and (identifier? lhs) (not (null? (cdr rhs*))))
               (raise-parse-error "right-hand-side is not a single expression" stx))
             (let loop ((lhs lhs) (rhs* rhs*))
               (if (identifier? lhs)
                   (values lhs (car rhs*))
                   (let ((lhs*~ (syntax->improper-list lhs)))
                     (unless (pair? lhs*~) (raise-parse-error "not a definable form" lhs))
                     (let ((lhs (car lhs*~)) (param (cdr lhs*~)))
                       (loop lhs (list (quasiquote-syntax (lambda #,param . #,rhs*)))))))))
           (syntax->list stx)))
  (define ((identifier-syntax op) stx)
    (let ((x (syntax-unwrap stx)))
      (cond ((symbol? x) (op stx))
            ((and (pair? x) (identifier? (car x))) (cons (op (car x)) (cdr x)))
            (else (raise-parse-error "not an identifier-syntax form" stx)))))
  (define ((identifier-only-syntax op) stx) (parse-identifier stx) (op stx)))

(define-syntax (define-vocabulary-syntax-binder stx)
  (apply (lambda (_ name bind-in-vocabulary vocabulary-name introduce-definitions? vocabulary-parser)
           (quasiquote-syntax
             (define-syntax (#,name stx)
               (let-values (((lhs rhs) (dismantle-operator-binding stx)))
                 (quasiquote-syntax
                   (#,(quote-syntax #,bind-in-vocabulary) #,lhs
                    #,(quote-syntax #,vocabulary-name)
                    ((make-macro-parser #,(quote-syntax #,introduce-definitions?)
                                        #,(quote-syntax #,vocabulary-parser))
                     (current-environment) #,rhs)))))))
         (syntax->list stx)))

(define-vocabulary-syntax-binder define-definition-syntax define-in-vocabulary vocab.definition #t parse-definition)
(define-vocabulary-syntax-binder define-expression-syntax define-in-vocabulary vocab.expression #f parse-expression)

(define-syntax (define-identifier-syntax stx)
  (let-values (((lhs rhs) (dismantle-operator-binding stx)))
    (quasiquote-syntax (define-syntax #,lhs (identifier-syntax #,rhs)))))
(define-syntax (identifier-syntax-rule stx)
  (apply (lambda (_ rhs) (quasiquote-syntax (identifier-syntax (lambda (_) (quote-syntax #,rhs)))))
         (syntax->list stx)))
(define-syntax (identifier-only-syntax-rule stx)
  (apply (lambda (_ rhs) (quasiquote-syntax (identifier-only-syntax (lambda (_) (quote-syntax #,rhs)))))
         (syntax->list stx)))
(define-syntax (define-identifier-syntax-rule stx)
  (apply (lambda (_ lhs rhs) (quasiquote-syntax (define-syntax #,lhs (identifier-syntax-rule #,rhs))))
         (syntax->list stx)))

(define-vocabulary-syntax-binder define-set!-syntax define-in-vocabulary vocab.set! #f parse-expression)
(define-vocabulary-syntax-binder add-set!-syntax    add-in-vocabulary    vocab.set! #f parse-expression)

(define-syntax (define-ref&set!-syntax stx)
  (apply (lambda (_ name transform-ref transform-set!)
           (quasiquote-syntax (begin (define-expression-syntax #,name #,transform-ref)
                                     (add-set!-syntax #,name #,transform-set!))))
         (syntax->list stx)))
(define-syntax (define-ref&set!-identifier-syntax stx)
  (apply (lambda (_ name transform-ref transform-set!)
           (quasiquote-syntax (define-ref&set!-syntax #,name
                                (identifier-syntax #,transform-ref)
                                (identifier-only-syntax #,transform-set!))))
         (syntax->list stx)))
(define-syntax (define-ref&set!-identifier-syntax-rule stx)
  (apply (lambda (_ name ref-result set!-result)
           (quasiquote-syntax (define-ref&set!-syntax #,name
                                (identifier-syntax-rule #,ref-result)
                                (identifier-only-syntax-rule #,set!-result))))
         (syntax->list stx)))
(define-syntax (add-set!-identifier-syntax stx)
  (apply (lambda (_ name transform-set!)
           (quasiquote-syntax (add-set!-syntax #,name (identifier-only-syntax #,transform-set!))))
         (syntax->list stx)))
(define-syntax (add-set!-identifier-syntax-rule stx)
  (apply (lambda (_ name transform-set!)
           (quasiquote-syntax (add-set!-syntax #,name (identifier-only-syntax-rule #,transform-set!))))
         (syntax->list stx)))
