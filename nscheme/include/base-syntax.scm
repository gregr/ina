(introduce current-environment)
;(declare-parser
  ;vocab.expression
  ;((current-environment env.op) env.use stx)
  ;(define (op env stx)
    ;(let ((top (syntax-unwrap stx)))
      ;(if (not (pair? top))
          ;(raise-syntax-error "must be used as an operator" stx)
          ;(if (not (null? (syntax-unwrap (cdr top))))
              ;(raise-syntax-error "expected no arguments" (cdr top))
              ;(list (quote-syntax quote) env.use)))))
  ;(transcribe-and-parse-expression env.use env.op op stx))

(define (list . xs) xs)

(define (not x) (if x #f #t))

(define (void . args) (values))

(introduce else =>)

(splicing-local
  ((begin-meta
     (define vocab.definition 'definition)
     (define vocab.expression 'expression)))

  (introduce define-syntax)
  (declare-parser
    vocab.definition
    ((define-syntax env.op) dst.use env.scope env.use stx)
    (define (op env stx)
      (define (def stx.lhs stx.rhs)
        (list (quote-syntax begin)
              (list (quote-syntax introduce) stx.lhs)
              (list (quote-syntax declare-parser) vocab.expression stx.lhs
                    (list (quote-syntax lambda) (quote-syntax (env.op))
                          (list (quote-syntax lambda) (quote-syntax (env.use stx))
                                (list (quote-syntax let) (list (list (quote-syntax op) stx.rhs))
                                      (quote-syntax
                                        (transcribe-and-parse-expression env.use env.op op stx))))))
              (list (quote-syntax declare-parser) vocab.definition stx.lhs
                    (list (quote-syntax lambda) (quote-syntax (env.op))
                          (list (quote-syntax lambda) (quote-syntax (dst.use env.scope env.use stx))
                                (list (quote-syntax let) (list (list (quote-syntax op) stx.rhs))
                                      (quote-syntax
                                        (transcribe-and-parse-definition
                                          dst.use env.scope env.use env.op op stx))))))))
      (define (loop stx.lhs stx.rhs*)
        (if (identifier? stx.lhs)
            (let ((tail (syntax-unwrap (cdr x))))
              (if (if (pair? tail) (if (null? (syntax-unwrap (cdr tail))) #f #t) #t)
                  (raise-syntax-error "define-syntax expects one right-hand-side expression" stx)
                  (def stx.lhs (car tail)))))
        (let ((lhs (syntax-unwrap stx.lhs)))
          (if (not (pair? lhs))
              (raise-syntax-error "not a definable form" stx.lhs)
              (loop (car lhs) (list (quote-syntax case-lambda) (cons (cdr lhs) stx.rhs*))))))
      (let ((top (syntax-unwrap stx)))
        (if (not (pair? top))
            (raise-syntax-error "must be used as an operator" stx)
            (let ((args (syntax-unwrap (cdr top))))
              (if (not (pair? args))
                  (raise-syntax-error "not a list of arguments" stx)
                  (loop (car args) (cdr args)))))))
    (transcribe-and-parse-definition dst.use env.scope env.use env.op op stx))

  (define-syntax (and stx)
    (let ((top (syntax-unwrap stx)))
      (if (not (pair? top))
          (raise-syntax-error "must be used as an operator" stx)
          (let ((args (syntax-unwrap (cdr top))))
            (if (null? args)
                (quote-syntax #t)
                (if (pair? args)
                    (if (null? (syntax-unwrap (cdr args)))
                        (car args)
                        (list (quote-syntax if) (car args)
                              (cons (quote-syntax and) (cdr args))
                              (quote-syntax #f)))
                    (raise-syntax-error "not a list" (cdr top))))))))

  (define-syntax (or stx)
    (let ((top (syntax-unwrap stx)))
      (if (not (pair? top))
          (raise-syntax-error "must be used as an operator" stx)
          (let ((args (syntax-unwrap (cdr top))))
            (if (null? args)
                (quote-syntax #f)
                (if (pair? args)
                    (if (null? (syntax-unwrap (cdr args)))
                        (car args)
                        (list (quote-syntax let) (list (list (quote-syntax result) (car args)))
                              (list (quote-syntax if) (quote-syntax result)
                                    (quote-syntax result)
                                    (cons (quote-syntax or) (cdr args)))))
                    (raise-syntax-error "not a list" (cdr top))))))))

  (define-syntax (cond stx)
    (lambda (lookup free-identifier=?)
      (define (loop stx.test stx*.body stx.cc*)
        ;; TODO: use lookup to determine fine-grained else?
        (if (and (identifier? stx.test) (free-identifier=? stx.test (quote-syntax else)))
            (if (not (null? cc*))
                (raise-syntax-error "else clause is not last" stx.test)
                (cons (quote-syntax let) (cons (quote-syntax ()) stx*.body)))
            (list (quote-syntax let) (list (list (quote-syntax result.test) stx.test))
                  (list (quote-syntax if) (quote-syntax result.test)
                        (if (null? stx*.body)
                            (quote-syntax (result.test))
                            (if (and (identifier? (car stx*.body))
                                     ;; TODO: use lookup to determine fine-grained =>?
                                     (free-identifier=? (car stx*.body) (quote-syntax =>)))
                                (if (or (null? (cdr stx*.body)) (pair? (cddr stx*.body)))
                                    (raise-syntax-error "arrow is not followed by one procedure"
                                                        (car stx*.body))
                                    (list (cadr stx*.body) (quote-syntax result.test)))
                                (cons (quote-syntax let) (cons (quote-syntax ()) stx*.body))))
                        (if (null? (syntax-unwrap stx.cc*))
                            (quote-syntax (values))
                            (cons (quote-syntax cond) stx.cc*))))))
      (let ((top (syntax-unwrap stx)))
        (if (not (pair? top))
            (raise-syntax-error "must be used as an operator" stx)
            (let ((args (syntax-unwrap (cdr top))))
              (if (null? args)
                  (raise-syntax-error "no clauses" stx)
                  (if (not (pair? args))
                      (raise-syntax-error "not a list of arguments" (cdr top))
                      (let ((cc (syntax->list? (car args))))
                        (if (not (pair? cc))
                            (raise-syntax-error "not a nonempty list" cc)
                            (loop (car cc) (cdr cc) (cdr args)))))))))))

  (introduce unsyntax unsyntax-splicing)
  (define-syntax (quasiquote-syntax stx)
    (lambda (lookup free-identifier=?)
      ;; TODO: use lookup to determine fine-grained quasiquote-syntax keywords?
      (define (keyword? stx) (and (identifier? stx)
                                  (or (free-identifier=? stx (quote-syntax unsyntax))
                                      (free-identifier=? stx (quote-syntax unsyntax-splicing))
                                      (free-identifier=? stx (quote-syntax quasiquote-syntax)))))
      (define (loop stx.qq level)
        (define (literal) (list (quote-syntax quote-syntax) stx.qq))
        (let ((qq (syntax-unwrap stx.qq)))
          (cond
            ((pair? qq)
             (let ((stx.qqa (car qq)) (stx.qqd (cdr qq)))
               (if (keyword? stx.qqa)
                   (cond ((syntax->list? stx.qqd)
                          => (lambda (stx*.qq)
                               (cond ((or (null? stx*.qq) (pair? (cdr stx*.qq)))
                                      (raise-syntax-error "not 1 argument" stx.qq))
                                     ((free-identifier=? stx.qqa (quote-syntax unsyntax))
                                      (cond ((= level 0) (car stx*.qq))
                                            (else (list (quote-syntax list) (quote-syntax unsyntax)
                                                        (loop (car stx*.qq) (- level 1))))))
                                     ((free-identifier=? stx.qqa (quote-syntax quasiquote-syntax))
                                      (list (quote-syntax list) (quote-syntax quasiquote-syntax)
                                            (loop (car stx*.qq) (+ level 1))))
                                     (else (raise-syntax-error "misplaced keyword" stx.qqa)))))
                         (else (raise-syntax-error "not a list" stx.qq)))
                   (let ((qqa (syntax-unwrap stx.qqa)))
                     (define (default)
                       (list (quote-syntax cons) (loop stx.qqa level) (loop stx.qqd level)))
                     (if (pair? qqa)
                         (let ((stx.qqaa (car qqa)))
                           (if (and (identifier? stx.qqaa)
                                    (free-identifier=? stx.qqaa
                                                       (quote-syntax unsyntax-splicing)))
                               (cond
                                 ((syntax->list? (cdr qqa))
                                  => (lambda (stx*.qq)
                                       (cond ((or (null? stx*.qq) (pair? (cdr stx*.qq)))
                                              (raise-syntax-error "not 1 argument" (cdr qqa)))
                                             ((= level 0) (list (quote-syntax append)
                                                                (list (quote-syntax syntax->list?)
                                                                      (car stx*.qq))
                                                                (loop qqd 0)))
                                             (else (list (quote-syntax cons)
                                                         (list (quote-syntax list)
                                                               (quote-syntax unsyntax-splicing)
                                                               (loop (car stx*.qq) (- level 1)))
                                                         (loop qqd (- level 1)))))))
                                 (else (default)))
                               (default)))
                         (default))))))
            ((vector? qq)      (let ((stx*.qq (vector->list qq)))
                                 (let ((result  (loop stx*.qq level)))
                                   (list (quote-syntax apply) (quote-syntax vector)
                                         (list (quote-syntax syntax->list?) result)))))
            ((keyword? stx.qq) (raise-syntax-error "misplaced keyword" stx.qq))
            (else              (literal)))))
      (let ((top (syntax->list? stx)))
        (if (not (pair? top))
            (raise-syntax-error "must be used as an operator" stx)
            (if (or (null? (cdr top)) (pair? (cddr top)))
                (raise-syntax-error "not 1 argument" stx)
                (loop (cadr top) 0))))))

  (splicing-local
    ((define-syntax (syntax-dismantle-clause stx)
       (define (loop stx.x stx.pattern stx.body stx.^fail)
         (let ((p (syntax-unwrap stx.pattern)))
           (cond ((pair? p)                 (quasiquote-syntax
                                              (let ((x.unwrap (syntax-unwrap #,stx.x)))
                                                (if (pair? x.unwrap)
                                                    (let ((xa (car x.unwrap)) (xd (cdr x.unwrap)))
                                                      (syntax-dismantle-clause
                                                        xa #,(car p)
                                                        (syntax-dismantle-clause
                                                          xd #,(cdr p) #,stx.body #,stx.^fail)
                                                        #,stx.^fail))
                                                    (#,stx.^fail)))))
                 ((identifier? stx.pattern) (quasiquote-syntax
                                              (let ((#,stx.pattern #,stx.x)) #,stx.body)))
                 (else                      (quasiquote-syntax
                                              (if (equal? (syntax-unwrap #,stx.x)
                                                          (quote #,stx.pattern))
                                                  #,stx.body (#,stx.^fail)))))))
       (let ((stx*.top (syntax->list? stx)))
         (if (not (pair? stx*.top))
             (raise-syntax-error "must be used as an operator" stx)
             (let ((stx*.args (cdr stx*.top)))
               (if (= (length stx*.args) 4)
                   (apply loop stx*.args)
                   (raise-syntax-error "not 4 arguments" stx)))))))
    (define-syntax (syntax-dismantle stx)
      (define (unique-pattern-variables?! stx.pattern)
        (void (let loop ((p (syntax-unwrap stx.pattern)) (id* '()))
                (cond ((identifier? stx.pattern)
                       (if (memp bound-identifier=? id*)
                           (raise-syntax-error "duplicate pattern variable" stx.pattern)
                           (cons stx.pattern id*)))
                      ((pair?   p) (loop (cdr p) (loop (car p) id*)))
                      ((vector? p) (raise-syntax-error "vector pattern not supported" stx.pattern))
                      (else        id*)))))
      (define (loop.clause stx.x stx.pattern stx*.body stx.^fail)
        (unique-pattern-variables?! stx.pattern)
        (quasiquote-syntax
          (syntax-dismantle-clause #,stx.x #,stx.pattern (let () . #,stx*.body) #,stx.fail)))
      (define (loop.clause* stx.x stx*.clauses)
        (cond
          ((null? stx*.clauses) (quasiquote-syntax (raise-syntax-error "no match" #,stx.x #,stx)))
          (else (let ((stx.clause (car stx*.clauses)) (stx*.clauses (cdr stx*.clauses)))
                  (cond
                    ((syntax->list? stx.clause)
                     => (lambda (stx*.parts)
                          (cond
                            ((null? stx*.parts)
                             (raise-syntax-error "missing pattern and body" stx.clause))
                            ((null? (cdr stx*.parts))
                             (raise-syntax-error "missing body" stx.clause))
                            (else (let ((stx.c0 (loop.clause stx.x (car stx*.parts) (cdr stx*.parts)
                                                             (quote-syntax fail)))
                                        (stx.c* (loop.clause* stx.x stx*.clauses)))
                                    (quasiquote-syntax (let ((fail (lambda () #,stx.c*)))
                                                         #,stx.c0)))))))
                    (else (raise-syntax-error "not a list" stx.clause)))))))
      (let ((top (syntax->list? stx)))
        (if (not (pair? top))
            (raise-syntax-error "must be used as an operator" stx)
            (let ((args (cdr top)))
              (if (null? args)
                  (raise-syntax-error "fewer than 1 argument" stx)
                  (quasiquote-syntax (let ((stx.x #,(car args)))
                                       #,(loop.clause* (quote-syntax stx.x) (cdr args))))))))))

  (define-syntax (when stx)
    (syntax-dismantle stx
      ((_ test . body*) (quasiquote-syntax (if #,test (let () . #,body*) (values))))))

  (define-syntax (unless stx)
    (syntax-dismantle stx
      ((_ test . body*) (quasiquote-syntax (when (not #,test) . #,body*)))))

  (splicing-local
    ((define-syntax (case-clause stx)
       (syntax-dismantle stx
         ((_ x (literals . body*) ^fail)
          (cond ((syntax->list? literals)
                 => (lambda (literal*) (quasiquote-syntax (if (member x (quote #,literals))
                                                              (let () . #,body*)
                                                              (#,^fail)))))
                (else (raise-syntax-error "not a list" literals)))))))
    (define-syntax (case stx)
      (lambda (lookup free-identifier=?)
        (syntax-dismantle stx
          ((_ e) (raise-syntax-error "no clauses" stx))
          ((_ e (literals . body*) . stx.clause*)
           (let ((clause* (syntax-unwrap stx.clause*)))
             ;; TODO: use lookup to determine fine-grained else?
             (if (free-identifier=? literals (quote-syntax else))
                 (cond ((null? clause*) (quasiquote-syntax (let ((x #,e)) . #,body*)))
                       ((pair? clause*) (raise-syntax-error "else clause is not last" literals))
                       (else            (raise-syntax-error "not a list" stx.clause*)))
                 (quasiquote-syntax (let ((x #,e))
                                      (let ((^fail #,(if (null? clause*)
                                                         (quasiquote-syntax (values))
                                                         (quasiquote-syntax
                                                           (lambda () (case x . #,stx.clause*))))))
                                        (case-clause x literals body* ^fail)))))))))))

  (introduce unquote unquote-splicing)
  (define-syntax (quasiquote stx)
    (lambda (lookup free-identifier=?)
      (define (loop stx.qq level)
        (let ((qq (syntax-unwrap stx.qq)))
          (cond
            ;; TODO: use lookup to determine fine-grained quasiquote keywords?
            ((and (identifier? stx.qq)
                  (or (free-identifier=? stx.qq (quote-syntax unquote))
                      (free-identifier=? stx.qq (quote-syntax unquote-splicing))
                      (free-identifier=? stx.qq (quote-syntax quasiquote))))
             (raise-syntax-error "misplaced keyword" stx.qq))
            ((vector?  qq)     (quasiquote-syntax
                                 (vector . #,(map (lambda (stx.qq) (loop stx.qq level))
                                                  (vector->list qq)))))
            (else (syntax-dismantle stx.qq
                    ((stx.qqa . stx.qqd)
                     (define (j1)
                       (define (j2) (quasiquote-syntax
                                      (cons #,(loop stx.qqa level) #,(loop stx.qqd level))))
                       (syntax-dismantle stx.qqd
                         ((stx.arg) (cond ((free-identifier=? stx.keyword (quote-syntax unquote))
                                           (if (= level 0)
                                               stx.arg
                                               (quasiquote-syntax
                                                 (unquote #,(loop stx.arg (- level 1))))))
                                          ((free-identifier=? stx.keyword (quote-syntax quasiquote))
                                           (quasiquote-syntax
                                             (quasiquote #,(loop stx.arg (+ level 1)))))
                                          (else (j2))))
                         (_ (j2))))
                     (syntax-dismantle stx.qqa
                       ((stx.keyword stx.spliced)
                        (cond ((not (free-identifier=? stx.keyword (quote-syntax unquote-splicing)))
                               (j1))
                              ((= level 0) (quasiquote-syntax
                                             (append #,stx.qqa #,(loop stx.qqd level))))
                              (else        (quasiquote-syntax
                                             ((unquote-splicing #,(loop stx.spliced (- level 1)))
                                              . #,(loop stx.qqd level))))))
                       (_ (j1))))
                    (_ (quasiquote-syntax (quote #,stx.qq))))))))
      (syntax-dismantle stx ((_ qq) (loop qq 0)))))

  (define-syntax (define-values stx)
    (syntax-dismantle stx
      ((_ lhs* rhs)
       (quasiquote-syntax
         (begin
           (define vec.value* (call-with-values #,rhs (case-lambda (#,lhs* (vector . #,lhs*)))))
           . #,(let ((lhs* (syntax->list lhs*)))
                 (map (lambda (i lhs) (quasiquote-syntax
                                        (define #,lhs (vector-ref vec.value* #,i))))
                      (iota (length lhs*)) lhs*)))))))

  (splicing-local
    ((begin-meta
       (define ((build-splicing/def def) stx)
         (syntax-dismantle stx
           ((_ stx.bpair* . body*)
            (cond ((syntax->list? stx.bpair*)
                   => (lambda (bpair*)
                        (quasiquote-syntax
                          (splicing-local
                            #,(map (lambda (stx.bpair)
                                     (syntax-dismantle stx.bpair
                                       ((lhs rhs) (quasiquote-syntax (#,def #,lhs #,rhs)))
                                       (_ (raise-syntax-error "not a binding pair" stx.bpair))))
                                   bpair*)
                            . #,body*))))
                  (else (raise-syntax-error "not a list of binding pairs" stx.bpair*))))))
       (define ((build-splicing-let/splicing-rec self splicing-rec) stx)
         (syntax-dismantle stx
           ((_ () outer inner body)
            (quasiquote-syntax
              (splicing-rec
                #,(reverse (syntax->list outer))
                (splicing-rec
                  #,(reverse (syntax->list inner))
                  #,body))))
           ((_ ((lhs rhs) . bpair*) outer inner body)
            (quasiquote-syntax
              (#,self bpair* ((temp #,rhs) . #,outer) ((#,lhs temp) . #,inner) #,body))))))
     (define-syntax build-splicing-let-syntax
       (build-splicing-let/splicing-rec (quote-syntax build-splicing-let-syntax)
                                        (quote-syntax splicing-letrec-syntax)))
     (define-syntax build-splicing-let
       (build-splicing-let/splicing-rec (quote-syntax build-splicing-let)
                                        (quote-syntax splicing-letrec)))
     (define-syntax build-splicing-let-values
       (build-splicing-let/splicing-rec (quote-syntax build-splicing-let-values)
                                        (quote-syntax splicing-letrec-values))))
    (define-syntax splicing-letrec-syntax  (build-splicing/def (quote-syntax define-syntax)))
    (define-syntax splicing-letrec*        (build-splicing/def (quote-syntax define)))
    (define-syntax splicing-letrec*-values (build-splicing/def (quote-syntax define-values)))
    (define-syntax (splicing-let-syntax stx)
      (syntax-dismantle stx
        ((_ bpair* . body*) (quasiquote-syntax
                              (build-splicing-let-syntax #,bpair* () () (let () . #,body*))))))
    (define-syntax (splicing-let stx)
      (syntax-dismantle stx
        ((_ bpair* . body*) (quasiquote-syntax
                              (build-splicing-let #,bpair* () () (let () . #,body*))))))
    (define-syntax (splicing-let-values stx)
      (syntax-dismantle stx
        ((_ bpair* . body*) (quasiquote-syntax
                              (build-splicing-let-values #,bpair* () () (let () . #,body*)))))))

  (define-syntax (splicing-letrec stx)
    (syntax-dismantle stx ((_ . args) (quasiquote-syntax (splicing-letrec* . #,args)))))
  (define-syntax (splicing-letrec-values stx)
    (syntax-dismantle stx ((_ . args) (quasiquote-syntax (splicing-letrec*-values . #,args)))))
  (define-syntax (splicing-let* stx)
    (syntax-dismantle stx
      ((_ ()               . body*) (quasiquote-syntax (splicing-let () . #,body*)))
      ((_ (bpair . bpair*) . body*) (quasiquote-syntax
                                      (splicing-let (#,bpair)
                                        (splicing-let* #,bpair* . #,body))))))
  (define-syntax (splicing-let*-values stx)
    (syntax-dismantle stx
      ((_ ()               . body*) (quasiquote-syntax (splicing-let () . #,body*)))
      ((_ (bpair . bpair*) . body*) (quasiquote-syntax
                                      (splicing-let-values (#,bpair)
                                        (splicing-let*-values #,bpair* . #,body))))))

  (define-syntax (local stx)
    (syntax-dismantle stx
      ((_ def* . body*) (quasiquote-syntax
                          (let () (splicing-local #,def* (let () . #,body*)))))))
  (define-syntax (letrec-syntax stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-letrec-syntax #,bpair* (let () . #,body*)))))))
  (define-syntax (let-syntax stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-let-syntax #,bpair* (let () . #,body*)))))))
  (define-syntax (letrec* stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-letrec* #,bpair* (let () . #,body*)))))))
  (define-syntax (letrec*-values stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-letrec*-values #,bpair* (let () . #,body*)))))))
  (define-syntax (let-values stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-let-values #,bpair* (let () . #,body*)))))))
  (define-syntax (let* stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-let* #,bpair* (let () . #,body*)))))))
  (define-syntax (let*-values stx)
    (syntax-dismantle stx
      ((_ bpair* . body*) (quasiquote-syntax
                            (let () (splicing-let*-values #,bpair* (let () . #,body*)))))))
  (define-syntax (letrec stx)
    (syntax-dismantle stx ((_ . args) (quasiquote-syntax (letrec* . #,args)))))
  (define-syntax (letrec-values stx)
    (syntax-dismantle stx ((_ . args) (quasiquote-syntax (letrec*-values . #,args)))))
  )
