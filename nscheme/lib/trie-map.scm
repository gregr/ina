((provide tmap:empty tmap->alist alist->tmap
          tmap-count tmap-find tmap-ref tmap-get tmap-add tmap-discard)
 (require test))

(define shift-size 4)
(define branch-size (arithmetic-shift 1 shift-size))
(define local-mask (- branch-size 1))
(define rshift-size (- shift-size))
(define (local-i i)   (bitwise-and i local-mask))
(define (shift-i i)   (arithmetic-shift i rshift-size))
(define (unshift-i i) (arithmetic-shift i shift-size))

(define (empty? t)  (null? t))
(define (leaf? t)   (pair? t))
(define (leaf i v)  (cons i v))
(define (leaf-i l)  (car l))
(define (leaf-v l)  (cdr l))
(define (branch? t) (vector? t))
(define (branch:single i0 v0)
  (define result (make-mvector branch-size '()))
  (mvector-set! result i0 v0)
  (mvector->vector result))
(define (branch-find b i) (vector-ref b i))
(define (branch-add b i v)
  (define result (make-mvector branch-size '()))
  (let copy ((ci (- branch-size 1)))
    (mvector-set! result ci (vector-ref b ci))
    (cond ((= ci 0) (mvector-set! result i v) (mvector->vector result))
          (#t       (copy (- ci 1))))))
(define (branch-discard-replace b i v)
  (if (branch? v) (branch-add b i v)
    (let loop ((j (- branch-size 1)) (found #f) (f-i i))
      (cond ((> 0 j) (define t (if (leaf? v) v found))
                     (leaf (+ (unshift-i (leaf-i t)) f-i) (leaf-v t)))
            ((= j i) (loop (- j 1) found f-i))
            (#t (define x (vector-ref b j))
             (cond ((empty? x)                       (loop (- j 1) found f-i))
                   ((or (leaf? v) found (branch? x)) (branch-add b i v))
                   (#t                               (loop (- j 1) x j))))))))

(define tmap:empty '())
(define (tmap->alist trie)
  (define (lift i t)
    (map (lambda (kv) (cons (+ (unshift-i (car kv)) i) (cdr kv)))
         (tmap->alist t)))
  (cond ((branch? trie) (apply append (map lift (range branch-size)
                                           (vector->list trie))))
        ((leaf? trie)   (list trie))
        (#t             '())))
(define (alist->tmap alist)
  (foldl (lambda (kv t) (tmap-add t (car kv) (cdr kv))) tmap:empty alist))
(define (tmap-count trie)
  (cond ((branch? trie)
         (let loop ((ci 0) (sz 0))
           (if (= branch-size ci) sz
             (loop (+ ci 1) (+ sz (tmap-count (branch-find trie ci)))))))
        ((leaf? trie) 1)
        (#t           0)))
(define (tmap-find trie i)
  (cond ((branch? trie) (tmap-find (branch-find trie (local-i i)) (shift-i i)))
        ((leaf? trie)   (if (= i (leaf-i trie)) trie tmap:empty))
        (#t             trie)))
(define (tmap-ref trie i)
  (define result (tmap-find trie i))
  (if (leaf? result) (leaf-v result) (error '"tmap-ref of missing index:" i)))
(define (tmap-get trie i default)
  (define result (tmap-find trie i))
  (if (leaf? result) (leaf-v result) default))
(define (tmap-add trie i v)
  (cond ((branch? trie)
         (define li (local-i i))
         (branch-add trie li (tmap-add (branch-find trie li) (shift-i i) v)))
        ((leaf? trie)
         (define i2 (leaf-i trie))
         (if (= i i2) (leaf i v)
           (tmap-add
             (branch:single (local-i i2) (leaf (shift-i i2) (leaf-v trie)))
             i v)))
        (#t (leaf i v))))
(define (tmap-discard trie i)
  (cond ((branch? trie)
         (define li  (local-i i))
         (define old (branch-find trie li))
         (define new (tmap-discard old (shift-i i)))
         (if (or (empty? old) (and (leaf? old) (leaf? new))) trie
           (branch-discard-replace trie li new)))
        ((leaf? trie) (if (= (leaf-i trie) i) tmap:empty trie))
        (#t           trie)))

;; TODO: union, intersection, difference?
;; Redefine tmap-add in terms of union, tmap-find in terms of intersection,
;; and tmap-discard in terms of difference?

(when test
  ;; TODO: move sort/k to another module.
  (define (sort/k xs)
    (define (list-odds xs)
      (if (null? xs) '()
        (cons (car xs) (if (null? (cdr xs)) '() (list-odds (cddr xs))))))
    (define (list-merge/k a b)
      (cond ((null? a)              b)
            ((null? b)              a)
            ((<= (caar a) (caar b)) (cons (car a) (list-merge/k (cdr a) b)))
            (#t                     (cons (car b) (list-merge/k a (cdr b))))))
    (if (or (null? xs) (null? (cdr xs))) xs
      (list-merge/k (sort/k (list-odds xs)) (sort/k (list-odds (cdr xs))))))

  (test 'sort/k-1
    (sort/k '((1 . a) (4 . d) (3 . c) (2 . b) (5 . e)))
    '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))

  (test 'tmap-identity-1
    (sort/k (tmap->alist (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e)))))
    '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e)))

  (test 'tmap-replace-1
    (sort/k (tmap->alist (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e) (0 . z)))))
    '((0 . z) (1 . b) (2 . c) (17 . d) (18 . e)))

  (test 'tmap-discard-1
    (sort/k (tmap->alist (tmap-discard (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))) 2)))
    '((0 . a) (1 . b) (17 . d) (18 . e)))
  (test 'tmap-discard-2
    (sort/k (tmap->alist (tmap-discard (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))) 18)))
    '((0 . a) (1 . b) (2 . c) (17 . d)))

  (test 'tmap-count-1
    (tmap-count (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))))
    5)
  (test 'tmap-count-2
    (tmap-count (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e) (0 . z))))
    5)

  (test 'tmap-get-1
    (tmap-get (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))) 3 'missing)
    'missing)

  (test 'tmap-ref-1
    (tmap-ref (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))) 1)
    'b)
  (test 'tmap-ref-2
    (tmap-ref (alist->tmap '((0 . a) (1 . b) (2 . c) (17 . d) (18 . e))) 17)
    'd)
  )
