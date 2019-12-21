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
    (`#(g:fail ,reasons) (lambda (k kf s) (kf s reasons)))
    (`#(g:eps  ,v-thunk) (lambda (k kf s) (k  s v-thunk)))
    (`#(g:one  ,meta ,?) (lambda (k kf s)
                           (let* ((s (s)) (v (car s)))
                             (if (? v) (k (cdr s) (thunk v))
                               (kf (cdr s) (list (cons v meta)))))))
    (`#(g:alt ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (k kf s)
        ;; TODO: should pass kf the shortest of s1 and s2.
        (p1 k (lambda (s1 r1) (p2 k (lambda (s2 r2) (kf s2 (append r1 r2)))
                                  s)) s)))
    (`#(g:seq ,g1 ,g2)
      (define p1 (grammar->parser/dfs g1))
      (define p2 (grammar->parser/dfs g2))
      (lambda (k kf s)
        (p1 (lambda (s r1) (p2 (lambda (s r2) (k s (thunk (cons (r1) (r2)))))
                               kf s))
            kf s)))
    (`#(g:app ,proc ,g)
      (define p (grammar->parser/dfs g))
      (lambda (k kf s) (p (lambda (s r) (k s (thunk (proc (r)))))
                          kf s)))
    (`#(g:bind ,v->g ,g)
      (define p (grammar->parser/dfs g))
      (lambda (k kf s)
        (p (lambda (s r) ((grammar->parser/dfs (v->g (r))) k s)) kf s)))
    (`#(g:peek     ,g) (define p (grammar->parser/dfs g))
                       (lambda (k kf s) (p (lambda (_ r) (k s r))
                                           (lambda (_ r) (kf s r)) s)))
    (`#(g:peek-not ,g) (define p (grammar->parser/dfs g))
                       (lambda (k kf s)
                         (p (lambda (_ r) (kf s (list r)))
                            (lambda (_ r) (k s (thunk #t))) s)))
    (`#(g:nonterminal ,meta ,gthunk)
      (lambda (k kf s) ((grammar->parser/dfs (gthunk)) k kf s)))))

(define (parse/dfs g in k kf)
  ;; TODO: this strategy retains uncommitted buffers for the entire parse.
  ;; Switching to BFS would allow us to commit progress of the slowest branch
  ;;   * more complicated, but buffers may be committed incrementally
  ;;     * peek node retains a buffer to provide to downstream consumer
  ;;     * max retained buffer size determines global uncommitted amount
  ;;   * this is closer to the incremental parsing we eventually want
  (define (stream i)
    (define v #f)
    (lambda () (cond (v    v)
                     (else (set! v (cons (in 'peek i) (stream (+ i 1))))
                           v))))
  (define s:initial (stream 0))
  (define (k/commit k)
    (lambda (s:final r)
      (let commit ((s s:initial))
        (cond ((eq? s:final s) (k r))
              (else (in 'get) (commit (cdr (s))))))))
  ((grammar->parser/dfs g) (k/commit k) (k/commit kf) s:initial))

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
