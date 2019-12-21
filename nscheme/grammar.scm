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
        ((list?   xs) (foldr (lambda (x alts) (g:alt (one x) alts)) (fail) xs))
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

(define (grammar->parser/dfs g)
  (match g
    (`#(g:fail ,reasons) (lambda (k kf . _)     (kf reasons)))
    (`#(g:eps  ,thunk  ) (lambda (k kf s peek?) (k s thunk)))
    (`#(g:one  ,meta ,?) (lambda (k kf s peek?)
                           (let* ((s (s peek?)) (v (car s)))
                             (if (? v) (k (cdr s) (thunk v))
                               (kf (list (cons v meta)))))))
    (`#(g:alt ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (k kf s peek?)
        (p1 k (lambda (r1) (p2 k (lambda (r2) (kf (append r1 r2))) s peek?))
            s peek?)))
    (`#(g:seq ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (k kf s peek?)
        (p1 (lambda (s r1) (p2 (lambda (s r2) (k s (thunk (cons (r1) (r2)))))
                               kf s peek?))
            kf s peek?)))
    (`#(g:app ,proc ,g)
      (define p (grammar->parser/dfs g))
      (lambda (k kf s peek?) (p (lambda (s r) (k s (thunk (proc (r)))))
                                kf s peek?)))
    (`#(g:bind ,v->g ,g)
      (define p (grammar->parser/dfs g))
      (lambda (k kf s peek?)
        (p (lambda (s r) ((grammar->parser/dfs (v->g (r))) k s peek?))
           kf s peek?)))
    (`#(g:peek     ,g) (define p (grammar->parser/dfs g))
                       (lambda (k kf s _) (p (lambda (_ r) (k s r)) kf s #t)))
    (`#(g:peek-not ,g) (define p (grammar->parser/dfs g))
                       (lambda (k kf s _)
                         (p (lambda (_ r) (kf (list r)))
                            (lambda (r)   (k s (thunk #t))) s #t)))
    (`#(g:nonterminal ,meta ,gthunk)
      (lambda (k kf s peek?) ((grammar->parser/dfs (gthunk)) k kf s peek?)))))

(define (parse/dfs g in k kf)
  ;; TODO: this commit strategy is too eager.  For a set of alternatives, we
  ;; should only commit the amount consumed by the chosen match.  We can fix
  ;; this in a few ways:
  ;;   * thread a progress counter telling us how much to commit at the end
  ;;     * a downside is the need to retain uncommitted buffers for longer
  ;;   * use BFS instead of DFS, committing progress of the slowest branch
  ;;     * more complicated, but buffers may be committed incrementally
  ;;       * peek node retains a buffer to provide to downstream consumer
  ;;       * max retained buffer size determines global uncommitted amount
  ;;     * this is closer to the incremental parsing we eventually want
  (define lookahead 0)
  (define (stream)
    (let ((v #f) (committed? #f))
      (lambda (peek?)
        (cond ((and v (or committed? peek?)) v)
              (v (set! committed? #t) (set! lookahead (- lookahead 1))
                 (in 'get) v)
              (peek? (set! v (cons (in 'peek lookahead) (stream)))
                     (set! lookahead (+ lookahead 1))
                     v)
              (else  (set! committed? #t)
                     (set! v (cons (in 'get)            (stream)))
                     v)))))
  ((grammar->parser/dfs g) (lambda (_ r) (k r)) kf (stream) #f))

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
