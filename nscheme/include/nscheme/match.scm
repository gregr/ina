(define vocab.pattern           'pattern)
(define vocab.pattern-operator  'pattern-operator)
(define vocab.pattern-auxiliary 'pattern-auxiliary)

(define pattern-auxiliary? (auxiliary?/vocab vocab.pattern-auxiliary))

(define P:any                  (vector 'P:any      #f))
(define P:none                 (vector 'P:none     #f))
(define (P:var      id)        (vector 'P:var      #f id))
(define (P:quote    stx.value) (vector 'P:quote    #f stx.value))
(define (P:?        $?)        (vector 'P:?        #f $?))
(define (P:app      $proc P)   (vector 'P:app      #f $proc P))
(define (P:and      P1 P2)     (vector 'P:and      #f P1 P2))
(define (P:or       P1 P2)     (vector 'P:or       #f P1 P2))
(define (P:not      P)         (vector 'P:not      #f P))
(define (P:cons     P1 P2)     (vector 'P:cons     #f P1 P2))
(define (P:vector   P*)        (vector 'P:vector   #f P*))
(define (P:ellipsis P)         (vector 'P:ellipsis #f P))
(define (P:ellipsis-append P.ellipsis P.suffix length.suffix)
  (vector 'P:ellipsis-append #f P.ellipsis P.suffix length.suffix))
(define (P:ellipsis-vector P*.prefix P.ellipsis P*.suffix)
  (vector 'P:ellipsis-vector #f P*.prefix P.ellipsis P*.suffix))

(splicing-local
  ((define (P-tag     P)     (vector-ref P 0))
   (define (P-tagged? P tag) (eqv? (P-tag P) tag)))
  (define (P:any?             P) (P-tagged? P 'P:any))
  (define (P:none?            P) (P-tagged? P 'P:none))
  (define (P:var?             P) (P-tagged? P 'P:var))
  (define (P:quote?           P) (P-tagged? P 'P:quote))
  (define (P:??               P) (P-tagged? P 'P:?))
  (define (P:app?             P) (P-tagged? P 'P:app))
  (define (P:and?             P) (P-tagged? P 'P:and))
  (define (P:or?              P) (P-tagged? P 'P:or))
  (define (P:not?             P) (P-tagged? P 'P:not))
  (define (P:cons?            P) (P-tagged? P 'P:cons))
  (define (P:vector?          P) (P-tagged? P 'P:vector))
  (define (P:ellipsis?        P) (P-tagged? P 'P:ellipsis))
  (define (P:ellipsis-append? P) (P-tagged? P 'P:ellipsis-append))
  (define (P:ellipsis-vector? P) (P-tagged? P 'P:ellipsis-vector)))

(define (P:var-identifier                p) (vector-ref p 2))
(define (P:quote-value                   p) (vector-ref p 2))
(define (P:?-predicate                   p) (vector-ref p 2))
(define (P:app-procedure                 p) (vector-ref p 2))
(define (P:app-inner                     p) (vector-ref p 3))
(define (P:and-left                      p) (vector-ref p 2))
(define (P:and-right                     p) (vector-ref p 3))
(define (P:or-left                       p) (vector-ref p 2))
(define (P:or-right                      p) (vector-ref p 3))
(define (P:not-inner                     p) (vector-ref p 2))
(define (P:cons-car                      p) (vector-ref p 2))
(define (P:cons-cdr                      p) (vector-ref p 3))
(define (P:vector-element*               p) (vector-ref p 2))
(define (P:ellipsis-inner                p) (vector-ref p 2))
(define (P:ellipsis-append-ellipsis      p) (vector-ref p 2))
(define (P:ellipsis-append-suffix        p) (vector-ref p 3))
(define (P:ellipsis-append-suffix-length p) (vector-ref p 4))
(define (P:ellipsis-vector-prefix        p) (vector-ref p 2))
(define (P:ellipsis-vector-ellipsis      p) (vector-ref p 3))
(define (P:ellipsis-vector-suffix        p) (vector-ref p 4))

(define (P-source P) (P-note P))
(define (P-note   P) (vector-ref P 1))
(define (P-annotate P note)
  (case (vector-length P)
    ((2) (vector (vector-ref P 0) note))
    ((3) (vector (vector-ref P 0) note (vector-ref P 2)))
    ((4) (vector (vector-ref P 0) note (vector-ref P 2) (vector-ref P 3)))
    ((5) (vector (vector-ref P 0) note (vector-ref P 2) (vector-ref P 3) (vector-ref P 4)))
    (else (error "not a P" P))))

(define ($p:annotate P note)  (P-annotate P note))
(define ($p:source   P stx)   ($p:annotate P stx))
(define $p:any                P:any)
(define $p:none               P:none)
(define ($p:var      id)      (P:var      id))
(define ($p:quote    v)       (P:quote    v))
(define ($p:?        $?)      (P:?        $?))
(define ($p:app      $proc P) (P:app      $proc P))
(define ($p:and      a b)     (P:and      a b))
(define ($p:or       a b)     (P:or       a b))
(define ($p:not      P)       (P:not      P))
(define ($p:cons     a b)     (P:cons     a b))
(define ($p:vector   P*)      (P:vector   P*))
(define ($p:ellipsis P)       (P:ellipsis P))
(define ($p:list     . P*)    (foldr $p:cons ($p:quote '()) P*))
(define ($p:ellipsis-append P.ellipsis P.suffix length.suffix)
  (P:ellipsis-append P.ellipsis P.suffix length.suffix))
(define ($p:ellipsis-vector P*.prefix P.ellipsis P*.suffix)
  (P:ellipsis-vector P*.prefix P.ellipsis P*.suffix))

(define (linear-pattern-compile P)
  (define (id*->id.acc* id*)
    (map (lambda (i) (string->symbol (string-append "acc." (number->string i))))
         (range (length id*))))
  (define ($$p:and*       p*)    (if (null? p*)
                                     $p:any
                                     (let loop ((p (car p*)) (p* (cdr p*)))
                                       (if (null? p*) p ($p:and p (loop (car p*) (cdr p*)))))))
  (define ($$p:and        . p*)  ($$p:and* p*))
  (define ($$p:quote      value) ($p:? ($lambda '(x) (lambda ($x) ($literal-equal? value $x)))))
  (define ($$p:cons       a b)   ($$p:and ($p:? ($quote pair?))
                                          ($p:app ($quote car) a) ($p:app ($quote cdr) b)))
  (define ($$p:vector-ref p $i)  ($p:app ($lambda '(x) (lambda ($x) ($vector-ref $x $i))) p))
  (define ($$p:vector     p*)
    (let ((len (length p*)))
      ($$p:and* (cons* ($p:? ($quote vector?))
                       ($p:? ($lambda '(x) (lambda ($x)
                                             ($eqv? ($vector-length $x) ($quote len)))))
                       (map $$p:vector-ref p* (map $quote (range len)))))))
  (define (P->linear-pattern&variable* P)
    (define id-set.empty            '())
    (define (id-set-add     id* id) (cons id id*))
    (define (id-set-member? id* id) (memp (lambda (x) (identifier=? id x)) id*))
    (let loop ((P P) (id* id-set.empty))
      (define return (let ((note (P-note P)))
                       (lambda (P id*) (values ($p:annotate P note) id*))))
      (cond
        ((or (P:any? P) (P:none? P) (P:?? P)) (values P id*))
        ((P:var? P)   (let ((id (P:var-identifier P)))
                        (unless id* (raise-parse-error "disallowed pattern variable" id))
                        (when (id-set-member? id* id)
                          (raise-parse-error "duplicate pattern variable" id))
                        (values P (id-set-add id* id))))
        ((P:quote? P) (return ($$p:quote (P:quote-value P)) id*))
        ((P:cons? P)
         (let ((P.a (P:cons-car P)) (P.d (P:cons-cdr P)))
           (if (P:ellipsis? P.a)
               (let ((P.a (P:ellipsis-inner P.a)))
                 (let ((len (let len+check ((P P.d) (len 0))
                              (if (P:cons? P)
                                  (if (P:ellipsis? (P:cons-car P))
                                      (raise-parse-error "too many ellipses"
                                                         (P-source (P:cons-car P)))
                                      (len+check (P:cons-cdr P) (+ len 1)))
                                  len))))
                   (let*-values (((P.a id*) (loop P.a id*))
                                 ((P.d id*) (loop P.d id*)))
                     (return ($p:ellipsis-append P.a P.d len) id*))))
               (let*-values (((P.a id*) (loop P.a id*))
                             ((P.d id*) (loop P.d id*)))
                 (return ($$p:cons P.a P.d) id*)))))
        ((P:vector? P)
         (let* ((P*  (P:vector-element* P))
                (Pe* (filter P:ellipsis? P*)))
           (cond ((pair? Pe*)
                  (when (pair? (cdr Pe*)) (raise-parse-error "too many ellipses" (P-source P)))
                  (let vloop ((P (car P*)) (P* (cdr P*)) (prefix* '()) (id* id*))
                    (if (P:ellipsis? P)
                        (let-values (((P id*) (loop (P:ellipsis-inner P) id*)))
                          (let vloop ((P* P*) (rP* '()) (id* id*))
                            (if (null? P*)
                                (return ($p:ellipsis-vector (reverse prefix*) P (reverse rP*)) id*)
                                (let-values (((P id*) (loop (car P*) id*)))
                                  (vloop (cdr P*) (cons P rP*) id*)))))
                        (let-values (((P id*) (loop P id*)))
                          (vloop (car P*) (cdr P*) (cons P prefix*) id*)))))
                 (else (let vloop ((P* P*) (rP* '()) (id* id*))
                         (cond ((null? P*) (return ($$p:vector (reverse rP*)) id*))
                               (else (let-values (((P id*) (loop (car P*) id*)))
                                       (vloop (cdr P*) (cons P rP*) id*)))))))))
        ((P:app? P) (let-values (((P.inner id*) (loop (P:app-inner P) id*)))
                      (return ($p:app (P:app-procedure P) P.inner) id*)))
        ((P:and? P) (let*-values (((P.l id*) (loop (P:and-left P) id*))
                                  ((P.r id*) (loop (P:and-right P) id*)))
                      (return ($p:and P.l P.r) id*)))
        ((P:or?  P) (let*-values
                      (((P.l id*.l) (loop (P:or-left P) (and id* id-set.empty)))
                       ((P.r id*.r) (loop (P:or-right P) (and id* id-set.empty))))
                      (define (check id*.0 id*.1)
                        (for-each
                          (lambda (id)
                            (unless (id-set-member? id*.1 id)
                              (raise-parse-error "pattern variable not in both disjuncts" id))
                            (when (id-set-member? id* id)
                              (raise-parse-error "duplicate pattern variable" id)))
                          id*.0))
                      (when id* (check id*.l id*.r) (check id*.r id*.l))
                      (return ($p:or P.l P.r)
                              (foldl (lambda (id id*) (id-set-add id* id)) id* id*.l))))
        ((P:not? P) (let-values (((P.inner _) (loop (P:not-inner P) #f)))
                      (return ($p:not P.inner) id*)))
        (else       (error "not a linear P" P)))))
  (define (linear-pattern-variables P)
    (let loop ((P P) (id* '()))
      (cond
        ((P:var?             P) (cons (P:var-identifier P) id*))
        ((P:app?             P) (loop (P:app-inner P) id*))
        ((P:and?             P) (loop (P:and-right P) (loop (P:and-left P) id*)))
        ((P:or?              P) (loop (P:or-left P) id*))
        ((P:ellipsis-append? P) (loop (P:ellipsis-append-suffix P)
                                      (loop (P:ellipsis-append-ellipsis P) id*)))
        ((P:ellipsis-vector? P) (foldl (lambda (P id*) (loop P id*))
                                       (loop (P:ellipsis-vector-ellipsis P) id*)
                                       (append (P:ellipsis-vector-prefix P)
                                               (P:ellipsis-vector-suffix P))))
        (else                   id*))))
  (define ((wrap note ^E) . args) ($annotate (apply ^E args) note))
  (define (($$?   $?)    succeed fail $x env) ($if ($call $? $x) (succeed env) (fail env)))
  (define (($$and ^a ^b) succeed fail $x env) (^a (lambda (env) (^b succeed fail $x env))
                                                  fail $x env))
  (define (($$or  ^a ^b) succeed fail $x env) (^a succeed (lambda (env) (^b succeed fail $x env))
                                                  $x env))
  (define (($$app $proc ^p) succeed fail $x env)
    ($let1 'x.app ($call $proc $x) (lambda ($x.app) (^p succeed fail $x.app env))))
  (let-values (((P id*) (P->linear-pattern&variable* P)))
    (values
      (let loop ((P P))
        (wrap
          (P-source P)
          (cond
            ((P:any?  P) (lambda (succeed fail $x env) (succeed env)))
            ((P:none? P) (lambda (succeed fail $x env) (fail    env)))
            ((P:var?  P) (let ((id (P:var-identifier P)))
                           (lambda (succeed fail $x env)
                             ($let1/env env id $x (lambda (env) (succeed env))))))
            ((P:??    P) ($$? (P:?-predicate P)))
            ((P:app?  P) ($$app (P:app-procedure P) (loop (P:app-inner P))))
            ((P:and?  P) ($$and (loop (P:and-left P)) (loop (P:and-right P))))
            ((P:or?   P) ($$or (loop (P:or-left P)) (loop (P:or-right P))))
            ((P:not?  P) (let ((^p (loop (P:not-inner P))))
                           (lambda (succeed fail $x env) (^p fail succeed $x env))))
            ((P:ellipsis-append? P)
             (let* ((P.a   (P:ellipsis-append-ellipsis P))
                    (^a    (loop P.a))
                    (^d    (loop (P:ellipsis-append-suffix P)))
                    (id*.a (linear-pattern-variables P.a))
                    (len.d (P:ellipsis-append-suffix-length P)))
               (lambda (succeed fail $x env)
                 ($let1 'len.pre.x ($- ($improper-length $x) ($quote len.d))
                        (lambda ($len.pre.x)
                          ($if ($< $len.pre.x ($quote 0))
                               (fail env)
                               ($call
                                 ($loop 'continue/reversed-prefix
                                        (lambda ($continue/reversed-prefix)
                                          ($lambda
                                            (list 'len.pre.x 'x* 'rprefix*)
                                            (lambda ($len.pre.x $x* $rprefix*)
                                              ($if ($< ($quote 0) $len.pre.x)
                                                   ($call $continue/reversed-prefix
                                                          ($- $len.pre.x ($quote 1))
                                                          ($cdr $x*)
                                                          ($cons ($car $x*) $rprefix*))
                                                   ($let1 'k.a* ($lambda/env env id*.a (lambda (env) (^d succeed fail $x* env)))
                                                          (lambda ($k.a*)
                                                            ($call*
                                                              ($loop 'loop.ellipsis
                                                                     (lambda ($loop.ellipsis)
                                                                       ($lambda (cons 'rx* (id*->id.acc* id*.a))
                                                                                (lambda ($rx* . $acc*)
                                                                                  ($if ($null? $rx*)
                                                                                       ($call* $k.a* $acc*)
                                                                                       (^a (lambda (env)
                                                                                             (apply
                                                                                               $call $loop.ellipsis ($cdr $rx*)
                                                                                               (map $cons
                                                                                                    (parse-expression* env id*.a)
                                                                                                    $acc*)))
                                                                                           fail ($car $rx*) env))))))
                                                              (cons* $rprefix* (map (lambda (_) ($quote '())) id*.a))))))))))
                                 $len.pre.x $x ($quote '()))))))))
            ((P:ellipsis-vector? P)
             (let* ((P*.pre       (P:ellipsis-vector-prefix P))
                    (P.ell        (P:ellipsis-vector-ellipsis P))
                    (P*.suf       (P:ellipsis-vector-suffix P))
                    (length.pre   (length P*.pre))
                    (length.suf   (length P*.suf))
                    (length.fixed (+ length.pre length.suf))
                    (^pre         (loop ($$p:and* (map $$p:vector-ref P*.pre (map $quote (range length.pre))))))
                    (id*.ell      (linear-pattern-variables P.ell)))
               (lambda (succeed fail $x env)
                 (let* (($length.pre ($quote length.pre)))
                   ($if ($vector? $x)
                        ($let1 'length.ell
                               ($- ($vector-length $x) ($quote length.fixed))
                               (lambda ($length.ell)
                                 (let ((succeed.pre
                                         (lambda (env)
                                           ($let1 'i.suf.0
                                                  ($+ $length.ell $length.pre)
                                                  (lambda ($i.suf.0)
                                                    (let* (($i*.suf (map (lambda (i) ($+ $i.suf.0 ($quote i))) (range length.suf)))
                                                           ($p*.suf (map $$p:vector-ref P*.suf $i*.suf))
                                                           (^suf    (loop ($$p:and* $p*.suf))))
                                                      ($let1 'k.ell
                                                             ($lambda/env env id*.ell (lambda (env) (^suf succeed fail $x env)))
                                                             (lambda ($k.ell)
                                                               (apply
                                                                 $call
                                                                 ($loop 'loop.ell
                                                                        (lambda ($loop.ell)
                                                                          ($lambda (cons 'i.ell (id*->id.acc* id*.ell))
                                                                                   (lambda ($i.ell . $acc*)
                                                                                     (let ((succeed.ell
                                                                                             (lambda (env)
                                                                                               (apply
                                                                                                 $call $loop.ell ($- $i.ell ($quote 1))
                                                                                                 (map $cons
                                                                                                      (parse-expression* env id*.ell)
                                                                                                      $acc*)))))
                                                                                       ($if ($< $i.ell $length.pre)
                                                                                            ($call* $k.ell $acc*)
                                                                                            ((loop ($$p:vector-ref P.ell $i.ell))
                                                                                             succeed.ell fail $x env)))))))
                                                                 ($- $i.suf.0 ($quote 1))
                                                                 (map (lambda (_) ($quote '())) id*.ell))))))))))
                                   ($if ($< $length.ell ($quote 0))
                                        (fail env)
                                        (^pre succeed.pre fail $x env)))))
                        (fail env))))))
            (else (error "not a compilable linear P" P)))))
      id*)))

(define (parse-pattern-any    _ __)                $p:any)
(define (parse-pattern-var    _ stx.id)            ($p:var stx.id))
(define (parse-pattern-quote  _ stx.value)         ($p:quote (syntax->datum stx.value)))
(define (parse-pattern-vector env . stx*)          ($p:vector (parse-pattern* env stx*)))
(define (parse-pattern-list   env . stx*)          (apply $p:list (parse-pattern* env stx*)))
(define (parse-pattern-cons   env stx.car stx.cdr) ($p:cons (parse-pattern env stx.car)
                                                            (parse-pattern env stx.cdr)))
(define (parse-pattern-cons* env stx . stx*)
  (let loop ((stx stx) (stx* stx*))
    (let ((P (parse-pattern env stx)))
      (cond ((null? stx*) P)
            ((pattern-auxiliary? '... env (car stx*))
             (let ((stx* (cdr stx*))
                   (P    ($p:source ($p:ellipsis P) (car stx*))))
               (if (null? stx*)
                   P
                   ($p:cons P (loop (car stx*) (cdr stx*))))))
            (else ($p:cons P (loop (car stx*) (cdr stx*))))))))
(define ((parse-pattern/connective $p:connect $p:null) env . stx*)
  (if (null? stx*)
      $p:null
      (let loop ((stx (car stx*)) (stx* (cdr stx*)))
        (let ((P (parse-pattern env stx)))
          (if (null? stx*) P ($p:connect P (loop (car stx*) (cdr stx*))))))))
(define parse-pattern-and (parse-pattern/connective $p:and $p:any))
(define parse-pattern-or  (parse-pattern/connective $p:or  $p:none))
(define (parse-pattern-not env stx) ($p:not (parse-pattern env stx)))
(define (parse-pattern-? env stx.? . stx*)
  (let ((P.? ($p:? (parse-expression env stx.?))))
    (if (null? stx*) P.? ($p:and P.? (apply parse-pattern-and env stx*)))))
(define (parse-pattern-app env stx.proc . stx*.subpattern)
  ($p:app (parse-expression env stx.proc) (apply parse-pattern-and env stx*.subpattern)))
(define (parse-pattern* env stx*)
  (if (null? stx*)
      '()
      (let loop ((stx (car stx*)) (stx* (cdr stx*)))
        (let ((P (parse-pattern env stx)))
          (cond ((null? stx*) (list P))
                ((pattern-auxiliary? '... env (car stx*))
                 (let ((stx (car stx*)) (stx* (cdr stx*)))
                   (cons ($p:source ($p:ellipsis P) stx)
                         (if (null? stx*) '() (loop (car stx*) (cdr stx*))))))
                (else (cons P (loop (car stx*) (cdr stx*)))))))))

(define (parse-pattern env stx)
  ($p:source
    (let ((x (syntax-unwrap stx)))
      (cond
        ((identifier? stx) (let ((op (env-vocabulary-ref env stx vocab.pattern)))
                             (if (procedure? op) (op env stx) ($p:var stx))))
        ((pair?    x)      (let ((e.op (car x)))
                             (parse-identifier e.op)
                             (let ((op (env-vocabulary-ref env e.op vocab.pattern-operator)))
                               (unless (procedure? op)
                                 (raise-unbound-identifier-parse-error
                                   "not a pattern operator" e.op vocab.pattern-operator env))
                               (op env stx))))
        ((literal? x)      ($p:quote x))
        (else              (raise-parse-error "not a pattern" stx))))
    stx))
(define (parse-pattern-quasiquote env stx.qq)
  (define (operand qq) (car (syntax-unwrap (cdr qq))))
  (define (operation? qq tag)
    (and (pair? qq)
         (let ((qq.d (syntax-unwrap (cdr qq))))
           (and (pair? qq.d)
                (let ((qq.dd (syntax-unwrap (cdr qq.d))))
                  (and (null? qq.dd)
                       (identifier? (car qq))
                       (eqv? (env-vocabulary-ref env (car qq) vocab.quasiquote) tag)))))))
  (define (tag tag-value P) ($p:list ($p:quote tag-value) P))
  (define (pqq* stx* level)
    (if (null? stx*)
        '()
        (let pqq.loop ((stx (car stx*)) (stx* (cdr stx*)))
          (let ((P (loop stx level)))
            (cond ((null? stx*) (list P))
                  ((pattern-auxiliary? '... env (car stx*))
                   (let ((stx (car stx*)) (stx* (cdr stx*)))
                     (cons ($p:source ($p:ellipsis P) stx)
                           (if (null? stx*) '() (pqq.loop (car stx*) (cdr stx*))))))
                  (else (cons P (pqq.loop (car stx*) (cdr stx*)))))))))
  (define (loop stx.qq level)
    (let ((qq (syntax-unwrap stx.qq)))
      (cond ((operation? qq 'unquote)    (let ((rand (operand qq)))
                                           (if (= level 0)
                                               (parse-pattern env rand)
                                               (tag 'unquote (loop rand (- level 1))))))
            ((operation? qq 'quasiquote) (tag 'quasiquote (loop (operand qq) (+ level 1))))
            ((pair?      qq)             (let (($p:a (loop (car qq) level))
                                               (qq.d (syntax-unwrap (cdr qq))))
                                           (if (and (pair? qq.d)
                                                    (pattern-auxiliary? '... env (car qq.d)))
                                               ($p:cons
                                                 ($p:source ($p:ellipsis $p:a) (car qq.d))
                                                 (loop (cdr qq.d) level))
                                               ($p:cons $p:a (loop (cdr qq) level)))))
            ((vector?    qq)             ($p:vector (pqq* (vector->list qq) level)))
            (else (when (and (identifier? stx.qq) (env-vocabulary-ref env stx.qq vocab.quasiquote))
                    (raise-parse-error "misplaced quasiquote pattern operator" stx.qq))
                  ($p:quote stx.qq)))))
  (loop stx.qq 0))

(define ((match-pattern-operator-parser parser argc.min argc.max) env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-parse-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-parse-error "too many operator arguments" stx))
    (apply parser env (cdr stx*))))

(define ((parse-match/parse-pattern parse-pattern) env stx.e . stx*.clause*)
  ($let1 'x (parse-expression env stx.e)
         (lambda ($x)
           ($let1 'fail ($thunk ($error ($quote "no matching clause") $x))
                  (lambda ($fail)
                    (let loop ((stx*.clause* stx*.clause*))
                      (if (null? stx*.clause*)
                          ($call $fail)
                          (let ((clause (syntax-unwrap (car stx*.clause*))))
                            (unless (and (pair? clause) (pair? (syntax-unwrap (cdr clause))))
                              (raise-parse-error "not a match clause" (car stx*.clause*)))
                            (let*-values (((stx.body)       (cdr clause))
                                          ((^pattern id*.P) (linear-pattern-compile (parse-pattern env (car clause)))))
                              ($let1 'fail ($thunk (loop (cdr stx*.clause*)))
                                     (lambda ($fail)
                                       ($let1 'succeed
                                              ($lambda/env
                                                env id*.P
                                                (lambda (env)
                                                  (let* ((body  (syntax-unwrap stx.body))
                                                         (test* (let ((fender (syntax-unwrap (car body))))
                                                                  (and (pair? fender)
                                                                       (expression-auxiliary? 'guard env (car fender))
                                                                       (syntax->list (cdr fender))))))
                                                    (cond ((not test*) (parse-body env stx.body))
                                                          ((pair? test*)
                                                           ($if (apply $and (parse-expression* env test*))
                                                                (parse-body env (cdr body))
                                                                ($call $fail)))
                                                          (else (raise-parse-error "not a guard" (car body)))))))
                                              (lambda ($succeed)
                                                ($source
                                                  (^pattern
                                                    (lambda (env) ($call* $succeed (parse-expression* env id*.P)))
                                                    (lambda (_) ($call $fail))
                                                    $x env)
                                                  (car stx*.clause*)))))))))))))))

(define parse-match  (parse-match/parse-pattern parse-pattern))
(define parse-qmatch (parse-match/parse-pattern parse-pattern-quasiquote))

(define (env-conjoin/match env)
  (let ((env.scope (make-env))
        (b*.expr-aux '(guard))
        (b*.pattern-aux '(...))
        (b*.expr
          (list (cons 'match  (expression-operator-parser parse-match  1 #f))
                (cons 'qmatch (expression-operator-parser parse-qmatch 1 #f))))
        (b*.match-pattern
          (list (cons '_ parse-pattern-any)))
        (b*.match-pattern-operator
          (list (cons 'var        (match-pattern-operator-parser parse-pattern-var        1 1))
                (cons 'quote      (match-pattern-operator-parser parse-pattern-quote      1 1))
                (cons 'quasiquote (match-pattern-operator-parser parse-pattern-quasiquote 1 1))
                (cons 'app        (match-pattern-operator-parser parse-pattern-app        2 #f))
                (cons '?          (match-pattern-operator-parser parse-pattern-?          1 #f))
                (cons 'and        (match-pattern-operator-parser parse-pattern-and        0 #f))
                (cons 'or         (match-pattern-operator-parser parse-pattern-or         0 #f))
                (cons 'not        (match-pattern-operator-parser parse-pattern-not        1 1))
                (cons 'cons       (match-pattern-operator-parser parse-pattern-cons       2 2))
                (cons 'vector     (match-pattern-operator-parser parse-pattern-vector     0 #f))
                (cons 'cons*      (match-pattern-operator-parser parse-pattern-cons*      1 #f))
                (cons 'list       (match-pattern-operator-parser parse-pattern-list       0 #f))))
        (b*.match-pattern-operator-primitive
          (list )))
    (for-each (lambda (id) (env-vocabulary-bind! env.scope id vocab.expression-auxiliary
                                                 (syntax->datum id)))
              b*.expr-aux)
    (for-each (lambda (id) (env-vocabulary-bind! env.scope id vocab.pattern-auxiliary (syntax->datum id)))
              b*.pattern-aux)
    (for-each (lambda (id op) (env-vocabulary-bind! env.scope id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (for-each (lambda (id op) (env-vocabulary-bind! env.scope id vocab.pattern op))
              (map car b*.match-pattern) (map cdr b*.match-pattern))
    (for-each (lambda (id op)
                (let ((v=>v (env-ref env id)))
                  (if v=>v
                      (env-set!  env.scope id (vocab-dict-set v=>v vocab.pattern-operator op))
                      (env-vocabulary-bind! env.scope id vocab.pattern-operator op))))
              (map car b*.match-pattern-operator) (map cdr b*.match-pattern-operator))
    (env-conjoin (env-read-only env.scope) env)))
