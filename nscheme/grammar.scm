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
(define-tuple* (g:fail        msg       )
               (g:eps         v         )
               (g:one         ?         )
               (g:alt         g1   g2   )
               (g:seq         g1   g2   )
               (g:app         proc g    )
               (g:bind        k    g    )
               (g:peek        g         )
               (g:peek-not    g         )
               (g:nonterminal meta thunk))

(define-syntax-rule (return x) (g:eps (thunk x)))
(define (fail . xs)           (g:fail xs))
(define any                   (g:one (lambda (_) #t)))
(define (one v)               (g:one (lambda (x) (equal? v x))))
(define (in-range low high)   (g:one (lambda (x) (and x (<= low x high)))))

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

;; TODO: first-order representation to support various parsing strategies
;; start with a DFS strategy that will work with non-left-recursive grammars
;; for more expressiveness, try
;;   BFS to handle left-recursion as simply as possible
;;   CPS memoization (i.e., tabling support for left-recursion)
;;   parsing with derivatives
;;   shift/reduce state machines
;;   compiling to a DCG logic program

;; TODO: push-based iterative parsing
;; ensure that pusher can distinguish states which peek vs consume
;;   components of intermediate states must indicate size of unconsumed buffer
;;   pushers only commit values that are consumed by at least one component
