(define cons* list*)
(define (mvector-fill! mv v)
  (let loop ((i (- (mvector-length mv) 1)))
    (when (<= 0 i) (mvector-set! mv i v) (loop (- i 1)))))
(define (mvector-copy!/ref mv start src src-start src-end ref)
  (let loop ((i src-start))
    (cond ((<= src-end i) (- i src-start))
          (else (mvector-set! mv (+ start (- i src-start)) (ref src i))
                (loop (+ i 1))))))
(define (mvector-copy! mv start src src-start src-end)
  (mvector-copy!/ref mv start src src-start src-end mvector-ref))
;(define (mvector-copy!/string mv start src src-start src-end)
  ;(mvector-copy!/ref mv start src src-start src-end string-ref))
(define (mvector-copy!/list mv start xs)
  (let loop ((i start) (xs xs)) (cond ((null? xs) (- i start))
                                      (else       (mvector-set! mv i (car xs))
                                                  (loop (+ i 1) (cdr xs))))))

(define-syntax (define-variant stx)
  (syntax-case stx ()
    ((_ (tag field ...))
     (andmap identifier? (syntax->list #'(tag field ...)))
     (let ((tag-string (symbol->string (syntax->datum #'tag)))
           (len        (length (syntax->list #'(tag field ...)))))
       (with-syntax
         ((dlen     (datum->syntax #'_ len))
          (d?       (datum->syntax
                      #'tag (string->symbol (string-append tag-string "?"))))
          ((df ...) (datum->syntax
                      #'_ (map (lambda (f)
                                 (datum->syntax
                                   f (string->symbol
                                       (string-append
                                         tag-string "-"
                                         (symbol->string (syntax->datum f))))))
                               (syntax->list #'(field ...)))))
          ((di ...) (datum->syntax #'_ (range 1 len))))
         #'(begin (define (tag field ...) (vector 'tag field ...))
                  (define (d? d) (and (vector? d) (= dlen (vector-length d))
                                      (eq? 'tag (vector-ref d 0))))
                  (define (df d) (vector-ref d di)) ...))))))
(define-syntax-rule (define-variant* vd vds ...)
  (begin (define-variant vd) (define-variant vds) ...))

(define-syntax (cset/x stx)
  (define (cunit cstx)
    (define c (syntax->datum cstx))
    (define (string-length s) (bytevector-length (string->utf8 s)))
    (define (string-ref s i) (bytevector-ref (string->utf8 s) i))
    (cond ((byte? c)                                 c)
          ;; TODO: do not use string-length or string-ref
          ((and (string? c) (= (string-length c) 1)) (string-ref c 0))
          (else (error "invalid codeunit:" cstx))))
  (syntax-case stx ()
    ((_ x)           #'#f)
    ((_ x #(c ...))  #'(or (cset/x x c) ...))
    ((_ x #f)        #'(not x))
    ((_ x (r0 . r1)) (let ((p0 (cunit #'r0)) (p1 (cunit #'r1)))
                       #`(and x (<= #,p0 x #,p1))))
    ((_ x p?)        (identifier? #'p?) #'(p? x))
    ((_ x cstx) (let ((c (syntax->datum #'cstx)))
                  (cond ((byte? c)   #'(eq? x c))
                        ((string? c) #`(or #,@(map (lambda (b) #`(eq? x #,b))
                                                   (bytevector->list (string->utf8 c)))))
                        (else        (error "invalid cset:" #'cstx)))))))

(define-syntax-rule (cset c) (lambda (x) (cset/x x c)))

(define-syntax (case/char stx)
  (syntax-case stx (else)
    ((_ e clause ...) (not (identifier? #'e))
                      #'(let ((x e)) (case/char x clause ...)))
    ((_ x)               #'(error "missing char case:" x))
    ((_ x (else e* ...)) #'(begin e* ...))
    ((_ x (cs e* ...) clause ...)
     #'(cond (((cset cs) x) e* ...) (else (case/char x clause ...))))))

(define-syntax cseq?
  (syntax-rules ()
    ((_ get pos c cs ...) (case/char (get pos)
                            (c    (cseq? get (+ pos 1) cs ...))
                            (else #f)))
    ((_ get pos)          pos)))


(define (rx-compile sre)
  (define (codepoint p)
    (cond ((unicode? p) p)
          ((string? p) (define ps (string->unicodes p))
                       (if (= (length ps) 1) (codepoint (car ps))
                         (error "invalid unicode codepoint:" p)))
          (else (error "invalid unicode codepoint:" p))))
  (define (charset?? e r) (when (car r) (error "invalid character set:" e)) r)
  (define (ranges-subtract rs1 rs2)
    (cond ((or (null? rs1) (null? rs2)) rs1)
          ((< (cdar rs1) (caar rs2))
           (cons (car rs1) (ranges-subtract (cdr rs1) rs2)))
          ((< (cdar rs2) (caar rs1)) (ranges-subtract rs1 (cdr rs2)))
          (else (append (if (<= (caar rs2) (caar rs1)) '()
                          (list (cons (caar rs1) (- (caar rs2) 1))))
                        (ranges-subtract
                          (append (if (<= (cdar rs1) (cdar rs2)) '()
                                    (list (cons (+ (cdar rs2) 1) (cdar rs1))))
                                  (cdr rs1))
                          (append (if (< (cdar rs1) (cdar rs2))
                                    (list (cons (+ (cdar rs1) 1) (cdar rs2)))
                                    '())
                                  (cdr rs2)))))))
  (define (ranges-merge rs1 rs2)
    (cond ((null? rs1) rs2)
          ((null? rs2) rs1)
          ((= (+ (cdar rs1) 1) (caar rs2))
           (ranges-merge (cdr rs1) (cons (cons (caar rs1) (cdar rs2))
                                         (cdr rs2))))
          ((= (+ (cdar rs2) 1) (caar rs1))
           (ranges-merge (cons (cons (caar rs2) (cdar rs1)) (cdr rs1))
                         (cdr rs2)))
          ((< (cdar rs1) (caar rs2))
           (cons (car rs1) (ranges-merge (cdr rs1) rs2)))
          ((< (cdar rs2) (caar rs1))
           (cons (car rs2) (ranges-merge rs1 (cdr rs2))))
          (else (cons (cons (min (caar rs1) (caar rs2))
                            (max (cdar rs1) (cdar rs2)))
                      (ranges-merge (cdr rs1) (cdr rs2))))))
  (define (maybe-charset->code i r)
    (if (car r) r
      (let* ((ranges (list->vector (cddr r)))
             (body (let loop ((start 0) (end (vector-length ranges)))
                     (and (< start end)
                          (let* ((n (+ start (>> (- end start) 1)))
                                 (r (vector-ref ranges n)))
                            `(if (< x ,(car r)) ,(loop start n)
                               (or (<= x ,(cdr r)) ,(loop (+ n 1) end)))))))
             (predicate `(lambda (x) ,(if (cadr r) `(or (not x) ,body)
                                        `(and x ,body)))))
        (cons (+ i 1) `(#(test ,(eval (datum->syntax #'_ predicate))
                           ,(cons (cons (cadr r) ranges) predicate)))))))

  (define address       0)
  (define name=>address '())
  (define initial-result
    (let loop ((i 0) (parents '()) (sre sre))
      (define (retry e) (loop i parents e))
      (define (codify/parents i ps e) (maybe-charset->code i (loop i ps e)))
      (define (codify i e) (codify/parents i parents e))
      (define (codify/store addr i ps e)
        (define r (codify/parents (+ i 1) ps e))
        (cons (+ (car r) 1) (append `(#(store ,addr)) (cdr r)
                                    `(#(store ,(+ addr 1))))))
      (match sre
        ((? string?)      (retry `(seq . ,(string->unicodes sre))))
        ('eof             (list #f #t))
        ('any             (list #f #f (cons unicode-min    unicode-max)))
        ((not (? pair?))  (define p (codepoint sre))
                          (list #f #f (cons p p)))
        (`(range ,c1 ,c2) (list #f #f (cons (codepoint c1) (codepoint c2))))
        (`(set ,cs) (retry `(or . ,(string->unicodes cs))))
        (`(~ ,ce)   (retry `(- any ,ce)))
        (`(- ,ce1 ,ce2) (let ((r1 (charset?? ce1 (loop i parents ce1)))
                              (r2 (charset?? ce2 (loop i parents ce2))))
                          (cons* #f (and (cadr r1) (not (cadr r2)))
                                 (ranges-subtract (cddr r1) (cddr r2)))))
        (`(or ,e1 ,e2)
          (let* ((r1 (loop (+ i 1) parents e1))
                 (j (if (not (car r1)) (+ i 3) (+ (car r1) 1)))
                 (r2 (loop j       parents e2)))
            (if (and (not (car r1)) (not (car r2)))
              (cons* #f (or (cadr r1) (cadr r2))
                     (ranges-merge (cddr r1) (cddr r2)))
              (let* ((r1 (maybe-charset->code (+ i        1) r1))
                     (r2 (maybe-charset->code (+ (car r1) 1) r2)))
                (cons (car r2) (append `(#(branch ,(+ i 1) ,(+ (car r1) 1)))
                                       (cdr r1) `(#(jump ,(car r2)))
                                       (cdr r2)))))))
        (`(seq)         (cons i '()))
        (`(seq ,e1 ,e2) (let* ((r1 (codify i        e1))
                               (r2 (codify (car r1) e2)))
                          (cons (car r2) (append (cdr r1) (cdr r2)))))
        (`(? ,e) (define r (codify (+ i 1) e))
                 (cons (car r) (cons `#(branch ,(+ i 1) ,(car r)) (cdr r))))
        (`(* ,e) (define r (codify (+ i 1) e))
                 (cons (+ (car r) 1)
                       (append `(#(branch ,(+ i 1) ,(+ (car r) 1)))
                               (cdr r) `(#(jump ,i)))))
        (`(+ ,e) (define r (codify i e))
                 (cons (+ (car r) 1)
                       (append (cdr r) `(#(branch ,i ,(+ (car r) 1))))))
        (`(?? ,e) (define r (codify (+ i 1) e))
                  (cons (car r) (cons `#(branch ,(car r) ,(+ i 1)) (cdr r))))
        (`(*? ,e) (define r (codify (+ i 1) e))
                  (cons (+ (car r) 1)
                        (append `(#(branch ,(+ (car r) 1) ,(+ i 1)))
                                (cdr r) `(#(jump ,i)))))
        (`(+? ,e) (define r (codify i e))
                  (cons (+ (car r) 1)
                        (append (cdr r) `(#(branch ,(+ (car r) 1) ,i)))))
        (`($ ,e) (define addr address)
                 (set! address (+ address 2))
                 (codify/store addr i parents e))
        (`($: ,name ,e)
          (define key (reverse (cons name parents)))
          (define kv (assoc key name=>address))
          (define addr
            (if kv (cdr kv) (begin (set! name=>address
                                     (cons (cons key address) name=>address))
                                   (set! address (+ address 2))
                                   (- address 2))))
          (codify/store addr i (cons name parents) e))
        (`($:: ,parent ,e)          (codify/parents i (cons parent parents) e))
        (`(set ,cs . ,cs*)          (retry `(or (set ,cs) (set . ,cs*))))
        (`(~ ,ce . ,ce*)            (retry `(~ (or ,ce . ,ce*))))
        (`(- ,ce1 ,ce2 . ,ce*)      (retry `(- (- ,ce1 ,ce2) . ,ce*)))
        (`(or ,e)                   (retry e))
        (`(or ,e . ,e*)             (retry `(or ,e (or . ,e*))))
        (`(seq ,e)                  (retry e))
        (`(seq ,e . ,e*)            (retry `(seq ,e (seq . ,e*))))
        (`($:  ,name      ,e . ,e*) (retry `($:  ,name      (seq ,e . ,e*))))
        (`($:: ,namespace ,e . ,e*) (retry `($:: ,namespace (seq ,e . ,e*))))
        (`(,(and (or '? '* '+ '?? '*? '+? '$) op) . ,e*)
          (retry `(,op (seq . ,e*)))))))

  (define result (cdr (maybe-charset->code 0 initial-result)))
  (define (num/tag tag) (foldl (lambda (i count) (if (eq? (vector-ref i 0) tag)
                                                   (+ count 1) count))
                               0 result))
  (define state-count  (num/tag 'test))
  (define branch-count (num/tag 'branch))
  (define code (make-mvector (+ (length result) 1) '#(halt)))
  (mvector-copy!/list code 0 result)
  (vector state-count branch-count address name=>address
          (mvector->vector code)))

(define (rx-interpret program get pos)
  (match-define `#(,state-count ,branch-count ,address-count ,_ ,code) program)
  (define halted #f)
  (define thread-pool
    (let* ((size (+ 1 branch-count state-count)) (pool (make-mvector size #f)))
      (let loop ((i (- size 1)))
        (when (<= 0 i)
          (mvector-set! pool i (make-mvector (+ address-count 2) #f))
          (loop (- i 1))))
      pool))
  (define thread-pool-next 0)
  (define (thread-pc t)         (mvector-ref  t (+ address-count 1)))
  (define (thread-pc-set! t pc) (mvector-set! t (+ address-count 1) pc))
  (define (thread-halt! t pos)
    (mvector-set! t address-count pos)
    (when halted (thread-free! halted))
    (set! halted t))
  (define (thread-new! t?)
    (define t (mvector-ref thread-pool thread-pool-next))
    (set! thread-pool-next (+ thread-pool-next 1))
    (if t? (mvector-copy! t 0 t? 0 (mvector-length t?)) (mvector-fill! t #f))
    t)
  (define (thread-free! t)
    (set! thread-pool-next (- thread-pool-next 1))
    (mvector-set! thread-pool thread-pool-next t))
  (define visited? (make-mvector (vector-length code) #f))
  (define visits   (make-mvector (vector-length code) #f))
  (define visit-count 0)
  (define (visit?! pc) (and (not (mvector-ref visited? pc))
                            (begin (mvector-set! visited? pc #t)
                                   (mvector-set! visits visit-count pc)
                                   (set! visit-count (+ visit-count 1))
                                   #t)))
  (define (visits-clear!)
    (for-each (lambda (vi) (mvector-set! visited? (mvector-ref visits vi) #f))
              (range visit-count))
    (set! visit-count 0))
  (define (continue! q qsz t pos pc k kh)
    (cond ((visit?! pc)
           (match (vector-ref code pc)
             (`#(branch ,pc1 ,pc2)
               (continue! q qsz (thread-new! t) pos pc1
                          (lambda (qsz) (continue! q qsz t pos pc2 k kh))
                          kh))
             (`#(jump ,pc)       (continue! q qsz t pos pc k kh))
             (`#(store ,address) (mvector-set! t address pos)
                                 (continue! q qsz t pos (+ pc 1) k kh))
             (`#(halt)           (thread-halt! t pos) (kh qsz))
             (_ (thread-pc-set! t pc) (mvector-set! q qsz t) (k (+ qsz 1)))))
          (else (thread-free! t) (k qsz))))

  (define q   (make-mvector state-count #f))
  (define q2  (make-mvector state-count #f))
  (define qsz (continue! q 0 (thread-new! #f) pos 0
                         (lambda (z) z) (lambda (z) z)))
  (let loop ((pos pos) (queue q) (queue-size qsz) (q2 q2))
    (define (consume pos char)
      (define q2-size
        (let pop ((qi 0) (q2sz 0))
          (define (k  q2sz) (pop (+ qi 1) q2sz))
          (define (kh q2sz) (let pop ((qi (+ qi 1)))
                              (cond ((= qi queue-size) q2sz)
                                    (else (thread-free! (mvector-ref queue qi))
                                          (pop (+ qi 1))))))
          (if (= qi queue-size) q2sz
            (let* ((t (mvector-ref queue qi)) (pc (thread-pc t)))
              (match (vector-ref code pc)
                (`#(test ,valid? ,code)
                  (cond ((not (valid? char)) (thread-free! t) (k q2sz))
                        (else (continue! q2 q2sz t pos (+ pc 1) k kh)))))))))
      (loop pos q2 q2-size queue))
    (visits-clear!)
    (unless (= queue-size 0)
      (define b (get pos))
      (if (not b) (consume pos #f)
        (let iter ((pos (+ pos 1)) (next (utf8->unicode b)))
          (cond ((procedure? next) (define b (get pos))
                                   (when b (iter (+ pos 1) (next b))))
                (next              (consume pos next)))))))
  halted)

;; TODO: optional recompilation into a DFA that can track submatches properly
;; rx-interpret runs atom:number ~15x slower than a handwritten state machine.
(define (rx sre)
  (define code (rx-compile sre))
  (case-lambda (()       (vector-ref code 3))
               ((in pos) (rx-interpret code (lambda (i) (in 'peek i)) pos))))

(define (grammar? x) (vector? x))
(define-variant*
  (g:fail        reasons     )
  (g:eps         v-thunk     )
  (g:one         meta ?      )
  (g:alt         g1   g2     )
  (g:seq         g1   g2     )
  (g:app         proc g      )
  (g:bind        v->g g      )
  (g:peek        g           )
  (g:peek-not    g           )
  (g:nonterminal meta g-thunk))

(define-syntax-rule (return x) (g:eps (thunk x)))
(define (fail . xs)            (g:fail (list xs)))
(define any                    (g:one (list 'any)   (lambda (_) #t)))
(define (one v)                (g:one (list 'one v) (lambda (x) (equal? v x))))
(define (in-range low high)    (g:one (list 'in-range low high)
                                      (lambda (x) (and x (<= low x high)))))

(define (g:lift x)
  (cond ((grammar? x) x)
        ((string?  x) (g:app list->string (g:lift (bytevector->list (string->utf8 x)))))
        ((list?    x) (foldr (lambda (d g) (g:seq (one d) g)) (return '()) x))
        (else         (one x))))
(define (seq* xs)
  (cond ((or (string? xs) (list? xs)) (g:lift xs))
        ((vector? xs) (g:app list->vector (g:lift (vector->list xs))))
        (else         (error "invalid seq* source" xs))))
(define (alt* xs)
  (cond ((string?     xs) (alt* (string->utf8     xs)))
        ((bytevector? xs) (alt* (bytevector->list xs)))
        ((vector?     xs) (alt* (vector->list     xs)))
        ((list?       xs) (foldr (lambda (x alts) (g:alt (one x) alts))
                                 (g:fail '()) xs))
        (else             (error "invalid alt* source" xs))))

(define (gfoldl proc g gs)
  (if (null? gs) (g:lift g)
    (gfoldl proc (proc (g:seq (g:lift g) (g:lift (car gs)))) (cdr gs))))
(define (gfoldr proc g gs)
  (if (null? gs) (g:lift g)
    (proc (g:seq (g:lift (car gs)) (gfoldr proc g (cdr gs))))))
(define (seq  g . gs)        (gfoldl (lambda (g) (g:app cdr g)) g gs))
(define (seq0 g . gs)        (gfoldl (lambda (g) (g:app car g)) g gs))
(define (app/seq  proc . gs) (g:app  (lambda (args) (apply proc args))
                                     (gfoldr (lambda (x) x) (return '()) gs)))
(define (bind/seq proc . gs) (g:bind (lambda (args) (apply proc args))
                                     (gfoldr (lambda (x) x) (return '()) gs)))

(define (alt g . gs) (if (null? gs) (g:lift g)
                       (g:alt (g:lift g) (apply alt (car gs) (cdr gs)))))

(define (peek     g . gs) (g:peek     (apply seq g gs)))
(define (not/peek g . gs) (g:peek-not (apply seq g gs)))
(define (not/one  g . gs) (seq (apply not/peek g gs) any))
(define (not/alt* xs)     (not/one (alt* xs)))

(define-syntax-rule (let/seq    ((p rhs) ...) body ...)
  (bind/seq (lambda (p ...) body ...) rhs ...))
(define-syntax-rule (let/return ((p rhs) ...) body ...)
  (app/seq  (lambda (p ...) body ...) rhs ...))

(define-syntax define-grammar
  (syntax-rules ()
    ((_ (lhs p ...) body ...)
     (define (lhs p ...) (g:nonterminal (list 'lhs p ...) (thunk body ...))))
    ((_ (lhs p ... . ps) body ...)
     (define (lhs p ... . ps)
       (g:nonterminal (list* 'lhs p ... ps) (thunk body ...))))
    ((_ lhs body ...) (define lhs (g:nonterminal 'lhs (thunk body ...))))))
(define-syntax-rule (define-grammar* (g ...) (gs ...) ...)
  (begin (define-grammar g ...)
         (define-grammar gs ...) ...))

(define (?/seq default . gs) (alt (apply seq gs)   (return default)))
(define (*/seq . gs)         (alt (apply +/seq gs) (return '())))
(define-grammar (+/seq . gs) (let/return ((x  (apply seq gs))
                                          (xs (apply */seq gs)))
                               (cons x xs)))

;; TODO: performance: try compiling to CPS instead
;; Static continuation vs. dynamic (call/return and fail-to-alt) continuations
;; A parser then has the same signature as the continuations you pass to it:
;;   (s v kreturn kfail) -> return-or-fail
(define (grammar->parser/dfs g)
  (define-variant* (mzero reasons) (choice s v retry))
  (define-syntax-rule
    (case/p e ((s v retry) success-body ...) ((reasons) failure-body ...))
    (let ((x e)) (cond ((mzero? x) (let ((reasons (mzero-reasons x)))
                                     failure-body ...))
                       (else       (let ((s     (choice-s     x))
                                         (v     (choice-v     x))
                                         (retry (choice-retry x)))
                                     success-body ...)))))
  (define (unit s v) (choice s v (thunk (mzero '()))))
  (define (transform p t)
    (lambda (s)
      (let loop ((result (p s)))
        (case/p result
          ((s v retry) (choice s (t v) (thunk (loop (retry)))))
          ((rs)        (mzero rs))))))
  (define (remember reasons retry)
    (case/p (retry)
      ((s v retry)    (choice s v (thunk (remember reasons retry))))
      ((more-reasons) (mzero (append reasons more-reasons)))))
  (define (mplus result retry)
    (case/p result
      ((s v retry^) (choice s v (thunk (mplus (retry^) retry))))
      ((reasons)    (remember reasons retry))))

  (define (g->p g)
    (match g
      (`#(g:fail ,reasons) (lambda (s) (mzero reasons)))
      (`#(g:eps  ,v-thunk) (lambda (s) (unit s v-thunk)))
      (`#(g:one  ,meta ,?) (lambda (s) (let* ((s (s)) (v (car s)))
                                         (if (? v) (unit (cdr s) (thunk v))
                                           (mzero (list (cons v meta)))))))
      (`#(g:alt ,g1 ,g2)
        (define p1 (g->p g1)) (define p2 (g->p g2))
        (lambda (s) (mplus (p1 s) (thunk (p2 s)))))
      (`#(g:seq ,g1 ,g2)
        (define p1 (g->p g1)) (define p2 (g->p g2))
        (lambda (s)
          (let loop ((result (p1 s)))
            (case/p result
              ((s v1 retry) (mplus ((transform p2 (lambda (v2)
                                                    (thunk (cons (v1) (v2)))))
                                    s)
                                   (thunk (loop (retry)))))
              ((rs)         (mzero rs))))))
      (`#(g:app ,proc ,g) (transform (g->p g)
                                     (lambda (v) (thunk (proc (v))))))
      (`#(g:bind ,v->g ,g)
        (define p (g->p g))
        (lambda (s)
          (let loop ((result (p s)))
            (case/p result
              ((s v retry) (mplus ((g->p (v->g (v))) s)
                                  (thunk (loop (retry)))))
              ((reasons)   (mzero reasons))))))
      (`#(g:peek ,g)
        (define p (g->p g))
        (lambda (s) (let rewind ((result (p s)))
                      (case/p result
                        ((_ v retry) (choice s v (thunk (rewind (retry)))))
                        ((reasons)   (mzero reasons))))))
      (`#(g:peek-not ,g) (define p (g->p g))
                         (lambda (s) (case/p (p s)
                                       ((_ v retry) (mzero (list v)))
                                       ((reasons)   (unit s (thunk #t))))))
      (`#(g:nonterminal ,meta ,g-thunk)
        (lambda (s) ((g->p (g-thunk)) s)))))
  (define parse (g->p g))
  (lambda (in k kf)
    ;; TODO: this strategy retains uncommitted buffers for the entire parse.
    ;; Switching to BFS would allow us to commit progress of the slowest branch
    ;;   * more complicated, but buffers may be committed incrementally
    ;;     * peek node retains a buffer to provide to downstream consumer
    ;;     * max retained buffer size determines global uncommitted amount
    ;;   * this is closer to the incremental parsing we eventually want
    (define i 0)
    (define (stream)
      (define v #f)
      (thunk (cond (v    v)
                   (else (set! v (cons (in 'peek i) (stream)))
                         (set! i (+ i 1))
                         v))))
    (define s:initial (stream))
    (case/p (parse s:initial)
      ((s:final v _) (let commit ((s s:initial))
                       (cond ((eq? s:final s) (k v))
                             (else (in 'get) (commit (cdr (s)))))))
      ((reasons) (let commit ((i i))
                   (unless (= i 0) (in 'get) (commit (- i 1))))
                 (kf reasons)))))

;; TODO: for more expressiveness, try
;;   BFS to handle left-recursion as simply as possible
;;   CPS memoization (i.e., tabling support for left-recursion)
;;   parsing with derivatives
;;   shift/reduce state machines
;;   compiling to a DCG logic program

;; TODO: push-based iterative parsing
;; ensure that pusher can distinguish states which peek vs consume
;;   components of intermediate states must indicate size of unconsumed buffer
;;   pushers only commit values that are consumed by at least one component

;; TODO: grammar compaction via smart constructors?
