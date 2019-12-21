;; TODO: move define-tuple[*]
(define-syntax (define-tuple stx)
  (syntax-case stx ()
    ((_ (tag field ...))
     (andmap identifier? (syntax->list #'(tag field ...)))
     (let ((tag-string (symbol->string (syntax->datum #'tag)))
           (len (length (syntax->list #'(tag field ...)))))
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
(define-syntax-rule (define-tuple* dd dds ...)
  (begin (define-tuple dd) (define-tuple dds) ...))

(define (grammar? x) (vector? x))
(define-tuple* (g:fail        reasons     )
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
        ((string?  x) (g:app list->string (g:lift (string->list x))))
        ((list?    x) (foldr (lambda (d g) (g:seq (one d) g)) (return '()) x))
        (else         (one x))))
(define (seq* xs)
  (cond ((or (string? xs) (list? xs)) (g:lift xs))
        ((vector? xs) (g:app list->vector (g:lift (vector->list xs))))
        (else         (error "invalid seq* source" xs))))
(define (alt* xs)
  (cond ((string? xs) (alt* (string->vector xs)))
        ((vector? xs) (alt* (vector->list   xs)))
        ((list?   xs) (foldr (lambda (x alts) (g:alt (one x) alts))
                             (g:fail '()) xs))
        (else         (error "invalid alt* source" xs))))

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


(define-tuple* (mzero reasons) (choice s v retry))
(define-syntax-rule
  (case/p e ((s v retry) success-body ...) ((reasons) failure-body ...))
  (let ((x e)) (cond ((mzero? x) (let ((reasons (mzero-reasons x)))
                                   failure-body ...))
                     (else       (let ((s     (choice-s     x))
                                       (v     (choice-v     x))
                                       (retry (choice-retry x)))
                                   success-body ...)))))
(define (grammar->parser/dfs g)
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

  (match g
    (`#(g:fail ,reasons) (lambda (s) (mzero reasons)))
    (`#(g:eps  ,v-thunk) (lambda (s) (unit s v-thunk)))
    (`#(g:one  ,meta ,?) (lambda (s) (let* ((s (s)) (v (car s)))
                                       (if (? v) (unit (cdr s) (thunk v))
                                         (mzero (list (cons v meta)))))))
    (`#(g:alt ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (s) (mplus (p1 s) (thunk (p2 s)))))
    (`#(g:seq ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (s)
        (let loop ((result (p1 s)))
          (case/p result
            ((s v1 retry) (mplus ((transform p2 (lambda (v2)
                                                  (thunk (cons (v1) (v2)))))
                                  s)
                                 (thunk (loop (retry)))))
            ((rs)         (mzero rs))))))
    (`#(g:app ,proc ,g) (transform (grammar->parser/dfs g)
                                   (lambda (v) (thunk (proc (v))))))
    (`#(g:bind ,v->g ,g)
      (define p (grammar->parser/dfs g))
      (lambda (s)
        (let loop ((result (p s)))
          (case/p result
            ((s v retry) (mplus ((grammar->parser/dfs (v->g (v))) s)
                                (thunk (loop (retry)))))
            ((reasons)   (mzero reasons))))))
    (`#(g:peek ,g)
      (define p (grammar->parser/dfs g))
      (lambda (s) (let rewind ((result (p s)))
                    (case/p result
                      ((_ v retry) (choice s v (thunk (rewind (retry)))))
                      ((reasons)   (mzero reasons))))))
    (`#(g:peek-not ,g) (define p (grammar->parser/dfs g))
                       (lambda (s) (case/p (p s)
                                     ((_ v retry) (mzero (list v)))
                                     ((reasons)   (unit s (thunk #t))))))
    (`#(g:nonterminal ,meta ,g-thunk)
      (lambda (s) ((grammar->parser/dfs (g-thunk)) s)))))

(define (parse/dfs g in k kf)
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
  (case/p ((grammar->parser/dfs g) s:initial)
    ((s:final v _) (let commit ((s s:initial))
                     (cond ((eq? s:final s) (k v))
                           (else (in 'get) (commit (cdr (s)))))))
    ((reasons) (let commit ((i i))
                 (unless (= i 0) (in 'get) (commit (- i 1))))
               (kf reasons))))

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
