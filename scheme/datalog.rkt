#lang racket/base
(provide
  db-empty
  datalog-rules
  datalog-facts
  datalog-link
  datalog-eval
  sorted-db
  )

(require
  racket/list
  racket/match
  racket/set
  )

(define set-empty (set))
(define hash-empty (hash))
(define db-empty hash-empty)

;; TODO: aggregates in head; other rule body constraints

(define (var? d) (symbol? d))
(define (constant? d)
  (or (boolean? d) (null? d) (number? d) (char? d) (string? d)
      (pair? d) (vector? d)))
(define (term? d) (or (constant? d) (var? d)))
(define (term-list? d)
  (or (null? d) (and (pair? d) (term? (car d)) (term-list? (cdr d)))))
(define (predicate? d) (and (pair? d) (symbol? (car d)) (term-list? (cdr d))))
(define (predicate-name p) (car p))
(define (predicate-param* p) (cdr p))

(struct rule (head positive negative) #:transparent)
(define (rule-name r) (car (rule-head r)))
(define (rule-deps-positive r) (list->set (map car (rule-positive r))))
(define (rule-deps-negative r) (list->set (map car (rule-negative r))))
(define (rule-empty head) (rule head '() '()))
(define (rule-add-positive r p)
  (match-define (rule head pos neg) r)
  (rule head (cons p pos) neg))
(define (rule-add-negative r n)
  (match-define (rule head pos neg) r)
  (rule head pos (cons n neg)))

(define (rule-body? r d)
  (match d
    ('() r)
    (`(,(? predicate? p) . ,body)
      (define rb (rule-body? r body))
      (and rb (rule-add-positive rb p)))
    (`((#f ,(? predicate? np)) . ,body)
      (define rb (rule-body? r body))
      (and rb (rule-add-negative rb np)))
    (_ (error "invalid rule body:" r d))))

(define (vars-predicate p) (list->set (filter var? (predicate-param* p))))
(define (vars-predicate* p*)
  (foldl set-union set-empty (map vars-predicate p*)))

(define (rule-vars-covered r)
  (match-define (rule head pos neg) r)
  (define missing (set-subtract (vars-predicate head) (vars-predicate* pos)))
  (when (not (set-empty? missing))
    (error "incomplete variable coverage:" missing r)))

(define (covered-rule? d)
  (define r (and (pair? d) (predicate? (car d))
                 (rule-body? (rule-empty (car d)) (cdr d))))
  (and r (rule-vars-covered r) r))

(define (predicate-arity-union a b)
  (for/fold
    ((result b)) (((name arity) (in-hash a)))
    (when (not (= arity (hash-ref b name arity)))
      (error "predicate arity mismatch:" name arity (hash-ref b name arity)))
    (hash-set result name arity)))
(define (predicate-arity-union* a a*) (foldl predicate-arity-union a a*))

(define (predicate-arity p) (hash (car p) (length (cdr p))))
(define (rule->predicate-arity r)
  (match-define (rule head pos neg) r)
  (predicate-arity-union*
    (predicate-arity-union*
      (predicate-arity head) (map predicate-arity pos))
         (map predicate-arity neg)))

(define (datalog-rules rules)
  (define r* (map (lambda (d)
                    (define r (covered-rule? d))
                    (when (not r) (error "invalid rule:" d))
                    r) rules))
  ;; raises error on failure
  (predicate-arity-union* hash-empty (map rule->predicate-arity r*))
  r*)

(define (datalog-link* r**)
  ;; raises error on failure
  (foldl (lambda (r* a*) (predicate-arity-union*
                           a* (map rule->predicate-arity r*)))
         hash-empty r**)
  (append* r**))
(define (datalog-link . r**) (datalog-link* r**))

(define (datalog-facts facts) (datalog-rules (map list facts)))

(define (walk st t) (if (var? t) (hash-ref st t t) t))
(define (walk* st t*) (map (lambda (t) (walk st t)) t*))
(define (unify* st ta* b*)
  (define (unify st a b) (if (var? a) (hash-set st a b) (and (equal? a b) st)))
  (and st (if (null? ta*) st
            (unify* (unify st (walk st (car ta*)) (car b*))
                    (cdr ta*) (cdr b*)))))

(define (db-union a b)
  (for/fold
    ((result b)) (((name t**) (in-hash a)))
    (hash-set result name (set-union t** (hash-ref result name set-empty)))))
(define (db-union* db*) (foldl db-union hash-empty db*))
(define (db-subtract a b)
  (for/fold
    ((result a)) (((name t**) (in-hash b)))
    (define new (set-subtract (hash-ref result name set-empty) t**))
    (if (set-empty? new)
      (hash-remove result name)
      (hash-set result name new))))

(define (datalog-eval db r*)
  (define (fixed-point-eval db r*)
    (define (rule-eval r)
      (define st*
        (let loop ((st hash-empty) (pos* (rule-positive r)))
          (if (null? pos*)
            (let nloop ((st st) (neg* (rule-negative r)))
              (if (null? neg*) (list st)
                (let ((name (predicate-name (car neg*)))
                      (t* (predicate-param* (car neg*))))
                  (for/fold ((st* '()))
                            ((b* (in-set (hash-ref db name set-empty))))
                            (append (if (unify* st t* b*) '()
                                      (nloop st (cdr neg*))) st*)))))
            (let ((name (predicate-name (car pos*)))
                  (t* (predicate-param* (car pos*))))
              (for/fold ((st* '()))
                        ((b* (in-set (hash-ref db name set-empty))))
                        (define st-new (unify* st t* b*))
                        (append (if st-new (loop st-new (cdr pos*))
                                  '()) st*))))))
      (define t* (cdr (rule-head r)))
      (if (null? st*) hash-empty
        (hash (rule-name r) (for/set ((st st*)) (walk* st t*)))))

    (define db-new (db-subtract (db-union* (map rule-eval r*)) db))
    (if (hash-empty? db-new) db
      (fixed-point-eval (db-union db-new db) r*)))

  (define (stratified-eval db n*-finished n*-later r*-later r*)
    (define (should-wait? r)
      (or (ormap
            (lambda (p) (not (set-member? n*-finished (predicate-name p))))
            (rule-negative r)) (set-member? n*-later (rule-name r))
          (ormap (lambda (p) (set-member? n*-later (predicate-name p)))
                 (rule-positive r))))
    (define r*-wait (filter should-wait? r*))
    (define n*-wait (list->set (map rule-name r*-wait)))
    (define r*-continue (filter (lambda (r) (not (should-wait? r))) r*))
    (cond ((pair? r*-wait)
           (stratified-eval db n*-finished (set-union n*-wait n*-later)
                            (append r*-wait r*-later) r*-continue))
          ((pair? r*-later) (error "unstratifiable:" n*-later))
          ((null? r*) db)
          (else (stratified-eval
                  (fixed-point-eval db r*)
                  (set-union (list->set (map rule-name r*)) n*-finished)
                  set-empty '() r*-later))))

  (stratified-eval db set-empty set-empty '() r*))


(define example-rules
  (datalog-rules
    '(
      ((path X Y) (edge X Y))
      ((path X Z) (path X Y) (path Y Z))
      ;; alternatively, one of these:
      ;; ((path X Z) (edge X Y) (path Y Z))
      ;; ((path X Z) (path X Y) (edge Y Z))

      ;; optional reflexivity
      ;((path X X) (edge X Y))
      ;((path X X) (edge Y X))
      )))

(define example-facts
  (datalog-facts
    '(
      (edge 'a 'b)
      (edge 'b 'c)
      (edge 'c 'd)
      ;; optional cycle
      ;(edge 'd 'a)
      )))

(define (pair<? a b)
  (or (any<? (car a) (car b))
      (and (not (any<? (car b) (car a)))
           (any<? (cdr a) (cdr b)))))
(define (vector<? a b)
  (or (< (vector-length a) (vector-length b))
      (let loop ((i 0))
        (and (< i (vector-length a))
             (or (any<? (vector-ref a i) (vector-ref b i))
                 (and (not (any<? (vector-ref b i) (vector-ref a i)))
                      (loop (+ i 1))))))))

(define comparators
  `((,null? . ,(lambda _ #f))
    (,boolean? . ,(lambda (a b) (not a)))
    (,number? . ,<)
    (,symbol? . ,symbol<?)
    (,char? . ,char<?)
    (,string? . ,string<?)
    (,pair? . ,pair<?)
    (,vector? . ,vector<?)))

(define (any<? a b)
  (define (find-comparator x)
    (let loop ((i 0) (c* comparators))
      (if (or (null? c*) ((caar c*) x)) (cons i (cdar c*))
        (loop (+ i 1) (cdr c*)))))
  (match-define (cons ia c<) (find-comparator a))
  (match-define (cons ib _) (find-comparator b))
  (or (< ia ib) (and (= ia ib) (c< a b))))

(define (sorted-db db)
  (define (sorted-set x*) (sort (set->list x*) any<?))
  (sort (map (lambda (kv) (cons (car kv) (sorted-set (cdr kv))))
             (hash->list db))
        (lambda (n m) (symbol<? (car n) (car m)))))

;; testing
(define (test)
  (sorted-db
    (datalog-eval db-empty (datalog-link example-rules example-facts))))
