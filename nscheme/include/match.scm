(define vocab.pattern              'pattern)
(define vocab.pattern-operator     'pattern-operator)
(define vocab.pattern-auxiliary    'pattern-auxiliary)

(define pattern-auxiliary? (auxiliary?/vocab vocab.pattern-auxiliary))

(define (pattern:any             pv)           (vector 'any      pv))
(define (pattern:none            pv)           (vector 'none     pv))
(define (pattern:var             pv id)        (vector 'var      pv id))
(define (pattern:quote           pv stx.value) (vector 'quote    pv stx.value))
(define (pattern:?               pv $?)        (vector '?        pv $?))
(define (pattern:app             pv $proc P)   (vector 'app      pv $proc P))
(define (pattern:and             pv P1 P2)     (vector 'and      pv P1 P2))
(define (pattern:or              pv P1 P2)     (vector 'or       pv P1 P2))
(define (pattern:not             pv P)         (vector 'not      pv P))
(define (pattern:cons            pv P1 P2)     (vector 'cons     pv P1 P2))
(define (pattern:vector          pv P*)        (vector 'vector   pv P*))
(define (pattern:ellipsis        pv P)         (vector 'ellipsis pv P))
(define (pattern:ellipsis-append pv P.ellipsis P.suffix length.suffix)
  (vector 'ellipsis-append pv P.ellipsis P.suffix length.suffix))
(define (pattern:ellipsis-vector pv P*.prefix P.ellipsis P*.suffix)
  (vector 'ellipsis-vector pv P*.prefix P.ellipsis P*.suffix))

(define (pattern:any?             p) (ast-tagged? p 'any))
(define (pattern:none?            p) (ast-tagged? p 'none))
(define (pattern:var?             p) (ast-tagged? p 'var))
(define (pattern:quote?           p) (ast-tagged? p 'quote))
(define (pattern:??               p) (ast-tagged? p '?))
(define (pattern:app?             p) (ast-tagged? p 'app))
(define (pattern:and?             p) (ast-tagged? p 'and))
(define (pattern:or?              p) (ast-tagged? p 'or))
(define (pattern:not?             p) (ast-tagged? p 'not))
(define (pattern:cons?            p) (ast-tagged? p 'cons))
(define (pattern:vector?          p) (ast-tagged? p 'vector))
(define (pattern:ellipsis?        p) (ast-tagged? p 'ellipsis))
(define (pattern:ellipsis-append? p) (ast-tagged? p 'ellipsis-append))
(define (pattern:ellipsis-vector? p) (ast-tagged? p 'ellipsis-vector))

(define (pattern:var-identifier                p) (vector-ref p 2))
(define (pattern:quote-value-syntax            p) (vector-ref p 2))
(define (pattern:?-predicate                   p) (vector-ref p 2))
(define (pattern:app-procedure                 p) (vector-ref p 2))
(define (pattern:app-inner                     p) (vector-ref p 3))
(define (pattern:and-left                      p) (vector-ref p 2))
(define (pattern:and-right                     p) (vector-ref p 3))
(define (pattern:or-left                       p) (vector-ref p 2))
(define (pattern:or-right                      p) (vector-ref p 3))
(define (pattern:not-inner                     p) (vector-ref p 2))
(define (pattern:cons-car                      p) (vector-ref p 2))
(define (pattern:cons-cdr                      p) (vector-ref p 3))
(define (pattern:vector-element*               p) (vector-ref p 2))
(define (pattern:ellipsis-inner                p) (vector-ref p 2))
(define (pattern:ellipsis-append-ellipsis      p) (vector-ref p 2))
(define (pattern:ellipsis-append-suffix        p) (vector-ref p 3))
(define (pattern:ellipsis-append-suffix-length p) (vector-ref p 4))
(define (pattern:ellipsis-vector-prefix        p) (vector-ref p 2))
(define (pattern:ellipsis-vector-ellipsis      p) (vector-ref p 3))
(define (pattern:ellipsis-vector-suffix        p) (vector-ref p 4))

(define $p:any                       (pattern:any      #f))
(define $p:none                      (pattern:none     #f))
(define ($p:var             id)      (pattern:var      #f id))
(define ($p:quote           v)       (pattern:quote    #f v))
(define ($p:?               $?)      (pattern:?        #f $?))
(define ($p:app             $proc P) (pattern:app      #f $proc P))
(define ($p:and             a b)     (pattern:and      #f a b))
(define ($p:or              a b)     (pattern:or       #f a b))
(define ($p:not             P)       (pattern:not      #f P))
(define ($p:cons            a b)     (pattern:cons     #f a b))
(define ($p:vector          P*)      (pattern:vector   #f P*))
(define ($p:ellipsis        P)       (pattern:ellipsis #f P))
(define ($p:list            . P*)    (foldr $p:cons ($p:quote '()) P*))
(define ($p:ellipsis-append P.ellipsis P.suffix length.suffix)
  (pattern:ellipsis-append #f P.ellipsis P.suffix length.suffix))
(define ($p:ellipsis-vector P*.prefix P.ellipsis P*.suffix)
  (pattern:ellipsis-vector #f P*.prefix P.ellipsis P*.suffix))

(define (linear-pattern-compile P)
  (define (raise-pattern-error msg P)
    (raise-syntax-error msg (syntax-provenance-set (ast-provenance-set P #f) (ast-provenance P))))
  (define ($$p:and        . p*)  (if (null? p*)
                                     $p:any
                                     (let loop ((p (car p*)) (p* (cdr p*)))
                                       (cond ((null? p*) p)
                                             (else       ($p:and p (loop (car p*) (cdr p*))))))))
  (define ($$p:quote      value) ($p:? (ast:lambda #f '(x) ($eqv? ($ref 'x) ($quote value)))))
  (define ($$p:cons       a b)   ($$p:and ($p:? ($prim 'pair?))
                                          ($p:app ($prim 'car) a) ($p:app ($prim 'cdr) b)))
  (define ($$p:vector-ref p $i)  ($p:app (ast:lambda #f '(x) ($vector-ref ($ref 'x) $i)) p))
  (define ($$p:vector     p*)    (let ((len (length p*)))
                                   (apply $$p:and ($p:? ($prim 'vector?))
                                          ($p:? (ast:lambda #f '(x)
                                                            ($eqv? ($vector-length ($ref 'x))
                                                                   ($quote len))))
                                          (map $$p:vector-ref p* (map $quote (iota len))))))
  (define (pattern->linear-pattern&variable* P)
    (define id-set.empty            '())
    (define (id-set-add     id* id) (cons id id*))
    (define (id-set-member? id* id) (memp (lambda (x) (bound-identifier=? id x)) id*))
    (let loop ((P P) (id* id-set.empty))
      (let ((pv (ast-provenance P)))
        (define (wrap P) (ast-provenance-add P pv))
        (cond
          ((or (pattern:any? P) (pattern:none? P) (pattern:?? P)) (values P id*))
          ((pattern:var? P)
           (let ((id (pattern:var-identifier P)))
             (unless id* (raise-syntax-error "disallowed pattern variable" id))
             (when (id-set-member? id* id)
               (raise-syntax-error "duplicate pattern variable" id))
             (values P (id-set-add id* id))))
          ((pattern:quote? P)
           (values (wrap (let loop ((stx.value (pattern:quote-value-syntax P)))
                           (let ((q (syntax-unwrap stx.value)))
                             ($provenance
                               (cond ((pair?   q) ($$p:cons (loop (car q)) (loop (cdr q))))
                                     ((vector? q) ($$p:vector (map loop (vector->list q))))
                                     (else        ($$p:quote q)))
                               stx.value))))
                   id*))
          ((pattern:cons? P)
           (let ((P.a (pattern:cons-car P)) (P.d (pattern:cons-cdr P)))
             (if (pattern:ellipsis? P.a)
                 (let ((P.a (pattern:ellipsis-inner P.a)))
                   (let ((len (let len+check ((P P.d) (len 0))
                                (cond ((pattern:cons? P)
                                       (when (pattern:ellipsis? (pattern:cons-car P))
                                         (raise-pattern-error "too many ellipses"
                                                              (pattern:cons-car P)))
                                       (len+check (pattern:cons-cdr P) (+ len 1)))
                                      (else len)))))
                     (let*-values (((P.a id*) (loop P.a id*))
                                   ((P.d id*) (loop P.d id*)))
                       (values (wrap ($p:ellipsis-append P.a P.d len)) id*))))
                 (let*-values (((P.a id*) (loop P.a id*))
                               ((P.d id*) (loop P.d id*)))
                   (values (wrap ($$p:cons P.a P.d)) id*)))))
          ((pattern:vector? P)
           (let* ((P*  (pattern:vector-element* P))
                  (Pe* (filter pattern:ellipsis? P*)))
             (cond ((pair? Pe*)
                    (when (pair? (cdr Pe*)) (raise-pattern-error "too many ellipses" P))
                    (let vloop ((P (car P*)) (P* (cdr P*)) (prefix* '()) (id* id*))
                      (cond ((pattern:ellipsis? P)
                             (let-values (((P id*) (loop (pattern:ellipsis-inner P) id*)))
                               (let vloop ((P* P*) (rP* '()) (id* id*))
                                 (cond ((null? P*)
                                        (values (wrap ($p:ellipsis-vector
                                                        (reverse prefix*) P (reverse rP*)))
                                                id*))
                                       (else (let-values (((P id*) (loop (car P*) id*)))
                                               (vloop (cdr P*) (cons P rP*) id*)))))))
                            (else (let-values (((P id*) (loop P id*)))
                                    (vloop (car P*) (cdr P*) (cons P prefix*) id*))))))
                   (else (let vloop ((P* P*) (rP* '()) (id* id*))
                           (cond ((null? P*) (values (wrap ($$p:vector (reverse rP*))) id*))
                                 (else (let-values (((P id*) (loop (car P*) id*)))
                                         (vloop (cdr P*) (cons P rP*) id*)))))))))
          ((pattern:app? P) (let-values (((P.inner id*) (loop (pattern:app-inner P) id*)))
                              (values (wrap ($p:app (pattern:app-procedure P) P.inner)) id*)))
          ((pattern:and? P) (let*-values (((P.l id*) (loop (pattern:and-left P) id*))
                                          ((P.r id*) (loop (pattern:and-right P) id*)))
                              (values (wrap ($p:and P.l P.r)) id*)))
          ((pattern:or?  P) (let*-values
                              (((P.l id*.l) (loop (pattern:or-left P) (and id* id-set.empty)))
                               ((P.r id*.r) (loop (pattern:or-right P) (and id* id-set.empty))))
                              (define (check id*.0 id*.1)
                                (for-each (lambda (id)
                                            (unless (id-set-member? id*.1 id)
                                              (raise-syntax-error
                                                "pattern variable not in both disjuncts" id))
                                            (when (id-set-member? id* id)
                                              (raise-syntax-error "duplicate pattern variable" id)))
                                          id*.0))
                              (when id* (check id*.l id*.r) (check id*.r id*.l))
                              (values (wrap ($p:or P.l P.r))
                                      (foldl (lambda (id id*) (id-set-add id* id)) id* id*.l))))
          ((pattern:not? P) (let-values (((P.inner _) (loop (pattern:not-inner P) #f)))
                              (values (wrap ($p:not P.inner)) id*)))
          (else             (raise-pattern-error "unsupported pattern" P))))))
  (define (linear-pattern-variables P)
    (let loop ((P P) (id* '()))
      (cond
        ((pattern:var?             P) (cons (pattern:var-identifier P) id*))
        ((pattern:app?             P) (loop (pattern:app-inner P) id*))
        ((pattern:and?             P) (loop (pattern:and-right P) (loop (pattern:and-left P) id*)))
        ((pattern:or?              P) (loop (pattern:or-left P) id*))
        ((pattern:ellipsis-append? P) (loop (pattern:ellipsis-append-suffix P)
                                            (loop (pattern:ellipsis-append-ellipsis P) id*)))
        ((pattern:ellipsis-vector? P) (foldl (lambda (P id*) (loop P id*))
                                             (loop (pattern:ellipsis-vector-ellipsis P) id*)
                                             (append (pattern:ellipsis-vector-prefix P)
                                                     (pattern:ellipsis-vector-suffix P))))
        (else                         id*))))
  (define ((wrap pv ^ast) . args) (ast-provenance-add (apply ^ast args) pv))
  (define (($$?   $?)    succeed fail $x env) ($if ($call $? $x) (succeed env) (fail env)))
  (define (($$and ^a ^b) succeed fail $x env) (^a (lambda (env) (^b succeed fail $x env))
                                                  fail $x env))
  (define (($$or  ^a ^b) succeed fail $x env) (^a succeed (lambda (env) (^b succeed fail $x env))
                                                  $x env))
  (define (($$app $proc ^p) succeed fail $x env)
    (let* ((x.app.addr (fresh-address 'x.app)) ($x.app ($ref x.app.addr)))
      (ast:let #f (list x.app.addr) (list ($call $proc $x)) (^p succeed fail $x.app env))))
  (let-values (((P id*) (pattern->linear-pattern&variable* P)))
    (values
      (let loop ((P P))
        (wrap
          (ast-provenance P)
          (cond
            ((pattern:any?  P) (lambda (succeed fail $x env) (succeed env)))
            ((pattern:none? P) (lambda (succeed fail $x env) (fail    env)))
            ((pattern:var?  P) (let ((id (pattern:var-identifier P)))
                                 (lambda (succeed fail $x env)
                                   ($let env (list id) (list $x) (lambda (env $x) (succeed env))))))
            ((pattern:??    P) ($$? (pattern:?-predicate P)))
            ((pattern:app?  P) ($$app (pattern:app-procedure P) (loop (pattern:app-inner P))))
            ((pattern:and?  P) ($$and (loop (pattern:and-left P)) (loop (pattern:and-right P))))
            ((pattern:or?   P) ($$or (loop (pattern:or-left P)) (loop (pattern:or-right P))))
            ((pattern:not?  P) (let ((^p (loop (pattern:not-inner P))))
                                 (lambda (succeed fail $x env) (^p fail succeed $x env))))
            ((pattern:ellipsis-append? P)
             (let* ((P.a         (pattern:ellipsis-append-ellipsis P))
                    (^a          (loop P.a))
                    (^d          (loop (pattern:ellipsis-append-suffix P)))
                    (id*.a       (linear-pattern-variables P.a))
                    (addr*.a.acc (iota (length id*.a)))
                    (len.d       (pattern:ellipsis-append-suffix-length P)))
               (lambda (succeed fail $x env)
                 (let* (($x*              ($ref 'x*))
                        ($prefix-length.x ($ref 'prefix-length.x))
                        ($rprefix*        ($ref 'rprefix*))
                        ($rx*             ($ref 'rx*))
                        ($acc*            (map $ref addr*.a.acc))
                        ($k.a* ($lambda env id*.a (lambda (env . _) (^d succeed fail $x* env))))
                        ($loop.ellipsis ($ref 'loop.ellipsis))
                        ($loop.ellipsis
                          (ast:letrec
                            #f '(loop.ellipsis)
                            (list (ast:lambda
                                    #f (cons 'rx* addr*.a.acc)
                                    ($if ($null? $rx*)
                                         (apply $call ($ref 'k.a*) $acc*)
                                         (^a (lambda (env)
                                               (apply
                                                 $call $loop.ellipsis ($cdr $rx*)
                                                 (map $cons (parse-expression* env id*.a) $acc*)))
                                             fail ($car $rx*) env))))
                            $loop.ellipsis))
                        ($continue/reversed-prefix ($ref 'continue/reversed-prefix))
                        ($continue/reversed-prefix
                          (ast:letrec
                            #f '(continue/reversed-prefix)
                            (list (ast:lambda
                                    #f (list 'prefix-length.x 'x* 'rprefix*)
                                    ($if ($< ($quote 0) $prefix-length.x)
                                         ($call $continue/reversed-prefix
                                                ($- $prefix-length.x ($quote 1))
                                                ($cdr $x*)
                                                ($cons ($car $x*) $rprefix*))
                                         (ast:let
                                           #f '(k.a*) (list $k.a*)
                                           (apply $call $loop.ellipsis $rprefix*
                                                  (map (lambda (_) ($quote '())) addr*.a.acc))))))
                            $continue/reversed-prefix)))
                   (ast:let
                     #f '(prefix-length.x) (list ($- ($improper-length $x) ($quote len.d)))
                     ($if ($< $prefix-length.x ($quote 0))
                          (fail env)
                          ($call $continue/reversed-prefix $prefix-length.x $x ($quote '()))))))))
            ((pattern:ellipsis-vector? P)
             (let* ((P*.prefix       (pattern:ellipsis-vector-prefix P))
                    (P.ellipsis      (pattern:ellipsis-vector-ellipsis P))
                    (P*.suffix       (pattern:ellipsis-vector-suffix P))
                    (length.prefix   (length P*.prefix))
                    (length.suffix   (length P*.suffix))
                    (length.fixed    (+ length.prefix length.suffix))
                    ($i.ellipsis     ($ref 'i.ellipsis))
                    ($i.suffix.start ($ref 'i.suffix.start))
                    (^prefix         (loop (apply $$p:and
                                                  (map $$p:vector-ref P*.prefix
                                                       (map $quote (iota length.prefix))))))
                    (^ellipsis       (loop ($$p:vector-ref P.ellipsis $i.ellipsis)))
                    (^suffix         (loop (apply $$p:and
                                                  (map $$p:vector-ref P*.suffix
                                                       (map (lambda (i)
                                                              ($+ $i.suffix.start ($quote i)))
                                                            (iota length.suffix))))))
                    (id*.ellipsis    (linear-pattern-variables P.ellipsis))
                    (addr*.acc       (iota (length id*.ellipsis))))
             (lambda (succeed fail $x env)
               (let* (($length.ellipsis ($ref 'length.ellipsis))
                      ($length.prefix   ($quote length.prefix))
                      ($acc*            (map $ref addr*.acc))
                      ($loop.ellipsis   ($ref 'loop.ellipsis))
                      (succeed.ellipsis
                        (lambda (env)
                          (apply $call $loop.ellipsis
                                 ($- $i.ellipsis ($quote 1))
                                 (map $cons (parse-expression* env id*.ellipsis) $acc*))))
                      (succeed.prefix
                        (lambda (env)
                          (ast:let*
                            #f '(i.suffix.start k.ellipsis)
                            (list ($+ $length.ellipsis $length.prefix)
                                  ($lambda env id*.ellipsis
                                           (lambda (env . _) (^suffix succeed fail $x env))))
                            (apply $call (ast:letrec
                                           #f '(loop.ellipsis)
                                           (list (ast:lambda
                                                   #f (cons 'i.ellipsis addr*.acc)
                                                   ($if ($< $i.ellipsis $length.prefix)
                                                        (apply $call ($ref 'k.ellipsis) $acc*)
                                                        (^ellipsis succeed.ellipsis fail $x env))))
                                           $loop.ellipsis)
                                   ($- $i.suffix.start ($quote 1))
                                   (map (lambda (_) ($quote '())) addr*.acc))))))
                 ($if ($vector? $x)
                      (ast:let
                        #f '(length.ellipsis) (list ($- ($vector-length $x) ($quote length.fixed)))
                        ($if ($< $length.ellipsis ($quote 0))
                             (fail env)
                             (^prefix succeed.prefix fail $x env)))
                      (fail env))))))
            (else (raise-pattern-error "unsupported pattern" P)))))
      id*)))

(define (parse-pattern-any    _ __)                $p:any)
(define (parse-pattern-var    _ stx.id)            ($p:var stx.id))
(define (parse-pattern-quote  _ stx.value)         ($p:quote stx.value))
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
                   (P    (pattern:ellipsis (syntax-provenance (car stx*)) P)))
               (if (null? stx*)
                   P
                   ($p:cons P (loop (car stx*) (cdr stx*))))))
            (else ($p:cons P (loop (car stx*) (cdr stx*))))))))
(define ((parse-pattern/connective $p:connect $p:null) env . stx*)
  (if (null? stx*)
      $p:null
      (let loop ((stx (car stx*)) (stx* (cdr stx*)))
        (let ((P (parse-pattern env stx)))
          (cond ((null? stx*) P)
                (else         ($p:connect P (loop (car stx*) (cdr stx*)))))))))
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
                   (cons (pattern:ellipsis (syntax-provenance stx) P)
                         (if (null? stx*) '() (loop (car stx*) (cdr stx*))))))
                (else (cons P (loop (car stx*) (cdr stx*)))))))))

(define (parse-pattern env stx)
  ($provenance
    (let ((x (syntax-unwrap stx)))
      (cond
        ((identifier? stx) (let ((op (env-ref^ env stx vocab.pattern)))
                             (if (procedure? op) (op env stx) ($p:var stx))))
        ((pair?    x)      (let* ((e.op (car x))
                                  (op   (and (identifier? e.op)
                                             (env-ref^ env e.op vocab.pattern-operator))))
                             (if (procedure? op)
                                 (op env stx)
                                 (raise-syntax-error "not a match pattern" stx))))
        ((literal? x)      ($p:quote x))
        (else              (raise-syntax-error "not a pattern" stx))))
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
                       (eq? (env-ref^ env (car qq) vocab.quasiquote) tag)))))))
  (define (tag tag-value P) ($p:list ($p:quote tag-value) P))
  (define (pqq* stx* level)
    (if (null? stx*)
        '()
        (let pqq.loop ((stx (car stx*)) (stx* (cdr stx*)))
          (let ((P (loop stx level)))
            (cond ((null? stx*) (list P))
                  ((pattern-auxiliary? '... env (car stx*))
                   (let ((stx (car stx*)) (stx* (cdr stx*)))
                     (cons (pattern:ellipsis (syntax-provenance stx) P)
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
                                               ($p:cons (pattern:ellipsis
                                                          (syntax-provenance (car qq.d)) $p:a)
                                                        (loop (cdr qq.d) level))
                                               ($p:cons $p:a (loop (cdr qq) level)))))
            ((vector?    qq)             ($p:vector (pqq* (vector->list qq) level)))
            (else (when (and (identifier? stx.qq) (env-ref^ env stx.qq vocab.quasiquote))
                    (raise-syntax-error "misplaced quasiquote pattern operator" stx.qq))
                  ($p:quote stx.qq)))))
  (loop stx.qq 0))

(define ((match-pattern-operator-parser parser argc.min argc.max) env stx)
  (let* ((stx* (syntax->list stx)) (argc (- (length stx*) 1)))
    (unless (<= argc.min argc)           (raise-syntax-error "too few operator arguments"  stx))
    (unless (<= argc (or argc.max argc)) (raise-syntax-error "too many operator arguments" stx))
    (apply parser env (cdr stx*))))

(define ((parse-match/parse-pattern parse-pattern) env stx.e . stx*.clause*)
  (let (($x ($ref 'x)) ($fail ($call ($ref 'fail))))
    (ast:let
      #f '(x) (list (parse-expression env stx.e))
      (ast:let
        #f '(fail) (list (ast:lambda #f '() ($pcall 'panic ($quote "no matching clause") $x)))
        (let loop ((stx*.clause* stx*.clause*))
          (cond
            ((null? stx*.clause*) $fail)
            (else
              (let ((clause (syntax-unwrap (car stx*.clause*))))
                (unless (and (pair? clause) (pair? (syntax-unwrap (cdr clause))))
                  (raise-syntax-error "not a match clause" (car stx*.clause*)))
                (let*-values (((stx.body)       (cdr clause))
                              ((^pattern id*.P) (linear-pattern-compile
                                                  (parse-pattern env (car clause)))))
                  (ast:let
                    #f '(fail) (list (ast:lambda #f '() (loop (cdr stx*.clause*))))
                    (ast:let
                      #f '(succeed)
                      (list ($lambda
                              env id*.P
                              (lambda (env . _)
                                (let* ((body  (syntax-unwrap stx.body))
                                       (test* (let ((fender (syntax-unwrap (car body))))
                                                (and (pair? fender)
                                                     (expression-auxiliary? 'guard env (car fender))
                                                     (syntax->list (cdr fender))))))
                                  (if test*
                                      (if (pair? test*)
                                          ($if (apply $and (parse-expression* env test*))
                                               (parse-body env (cdr body))
                                               $fail)
                                          (raise-syntax-error "not a guard" (car body)))
                                      (parse-body env stx.body))))))
                      ($provenance
                        (^pattern
                          (lambda (env) (apply $call ($ref 'succeed) (parse-expression* env id*.P)))
                          (lambda (_) $fail)
                          $x env)
                        (car stx*.clause*)))))))))))))

(define parse-match  (parse-match/parse-pattern parse-pattern))
(define parse-qmatch (parse-match/parse-pattern parse-pattern-quasiquote))

(define (env-extend.match env)
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
    (for-each (lambda (id) (env-bind! env.scope id vocab.expression-auxiliary (syntax-peek id)))
              b*.expr-aux)
    (for-each (lambda (id) (env-bind! env.scope id vocab.pattern-auxiliary (syntax-peek id)))
              b*.pattern-aux)
    (for-each (lambda (id op) (env-bind! env.scope id vocab.expression-operator op))
              (map car b*.expr) (map cdr b*.expr))
    (for-each (lambda (id op) (env-bind! env.scope id vocab.pattern op))
              (map car b*.match-pattern) (map cdr b*.match-pattern))
    (for-each (lambda (id op)
                (let ((v=>v (env-ref env id)))
                  (if v=>v
                      (env-set!  env.scope id (vocab-dict-set v=>v vocab.pattern-operator op))
                      (env-bind! env.scope id vocab.pattern-operator op))))
              (map car b*.match-pattern-operator) (map cdr b*.match-pattern-operator))
    (env-freeze! env.scope)
    (env-extend env env.scope)))
