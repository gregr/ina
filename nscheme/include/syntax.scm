;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (vector #t))
(define antimark #f)
(define mark=?   eq?)
(define (mark*=? a* b*)
  (let loop ((a* a*) (b* b*))
    (if (pair? a*)
        (and (pair? b*)
             (mark=? (car a*) (car b*))
             (loop (cdr a*) (cdr b*)))
        (null? b*))))

(splicing-local
  ((define antimark? not)
   (define (marks-append m*.outer m*.inner)
     (cond ((null? m*.inner)                 m*.outer)
           ((null? m*.outer)                 m*.inner)
           ((not (antimark? (car m*.inner))) (append m*.outer m*.inner))
           (else (let loop ((m (car m*.outer)) (m* (cdr m*.outer)))
                   (cond ((null? m*) (cdr m*.inner))
                         (else       (cons m (loop (car m*) (cdr m*)))))))))

   (define rtd.annotated (make-rtd 'syntax:annotated #f #t '#(form provenance)))
   (define (make-annotated form provenance)
     ((record-constructor rtd.annotated) form provenance))
   (define (annotated form provenance)
     (if (not provenance) form (make-annotated form provenance)))
   (define annotated?           (record-predicate rtd.annotated))
   (define annotated-form       (record-field-name-accessor rtd.annotated 'form))
   (define annotated-provenance (record-field-name-accessor rtd.annotated 'provenance))
   (define (maybe-annotated-form       s) (if (annotated? s) (annotated-form s) s))
   (define (maybe-annotated-provenance s) (and (annotated? s) (annotated-provenance s)))
   (define (provenance-combine pv1 pv2)
     (cond ((not pv1)     pv2)
           ((not pv2)     pv1)
           ((eq? pv1 pv2) pv1)
           (else          (cons pv1 pv2))))

   (define rtd.marked (make-rtd 'syntax:marked #f #t '#(mark* form)))
   (define (make-marked mark* form) ((record-constructor rtd.marked) mark* form))
   (define (marked      mark* form) (if (null? mark*) form (make-marked mark* form)))
   (define marked?      (record-predicate rtd.marked))
   (define marked-mark* (record-field-name-accessor rtd.marked 'mark*))
   (define marked-form  (record-field-name-accessor rtd.marked 'form)))

  (define (syntax-mark*      s) (if (marked? s) (marked-mark* s) '()))
  (define (syntax-peek       s) (maybe-annotated-form       (if (marked? s) (marked-form s) s)))
  (define (syntax-provenance s) (maybe-annotated-provenance (if (marked? s) (marked-form s) s)))
  (define (syntax-provenance-set s pv)
    (if pv (marked (syntax-mark* s) (annotated (syntax-peek s) pv)) s))
  (define (syntax-provenance-add s pv)
    (syntax-provenance-set s (provenance-combine pv (syntax-provenance s))))

  (define (syntax-wrap s m*)
    (if (marked? s)
        (marked (marks-append m* (syntax-mark* s)) (marked-form s))
        (marked m* s)))

  (define (syntax-unwrap s)
    (if (marked? s)
        (let ((d (syntax-peek s)) (m* (syntax-mark* s)))
          (define (wrap x) (syntax-wrap x m*))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        (maybe-annotated-form s)))

  (define (syntax-remove-mark? s m)
    (let ((m* (syntax-mark* s)))
      (and (pair? m*)
           (mark=? (car m*) m)
           (marked (cdr m*) (marked-form s))))))

(define (identifier?  s) (symbol? (syntax-peek s)))
(define (identifier?! s) (unless (identifier? s) (error "not an identifier" s)))

(define (syntax-add-mark s m) (syntax-wrap s (list m)))

(define (free-identifier=?/env env a b)
  (identifier?! a) (identifier?! b)
  (let ((v=>v.a (env-ref env a)) (v=>v.b (env-ref env b)))
    (if (or v=>v.a v=>v.b)
        (eq? v=>v.a v=>v.b)
        (eq? (syntax-peek a) (syntax-peek b)))))

(define (bound-identifier=? a b)
  (identifier?! a) (identifier?! b)
  (and (eq? (syntax-peek a) (syntax-peek b))
       (mark*=? (syntax-mark* a) (syntax-mark* b))))

(define (datum->syntax context datum)
  (identifier?! context)
  (syntax-wrap datum (syntax-mark* context)))

(define (syntax->datum x)
  (let loop ((x x))
    (let strip ((d0 (syntax-peek x)))
      (cond ((pair?   d0) (cons (loop (car d0)) (loop (cdr d0))))
            ((vector? d0) (vector-map loop d0))
            (else         d0)))))

(define (syntax->improper-list s)
  (let ((x (syntax-unwrap s)))
    (if (pair? x)
        (cons (car x) (syntax->improper-list (cdr x)))
        x)))

(define (syntax->list? s)
  (let loop ((s s) (parts '()))
    (let ((x (syntax-unwrap s)))
      (if (null? x)
          (reverse parts)
          (and (pair? x)
               (loop (cdr x) (cons (car x) parts)))))))

(define (syntax->list s) (or (syntax->list? s) (error "not a list" s)))

;;;;;;;;;;;;;;;;;;;;
;;; Environments ;;;
;;;;;;;;;;;;;;;;;;;;

;; NOTE: lookup is currently O(n^2), but may not be a problem in practice.  Consider more efficient
;; dictionary data structures If we need to harden this.
;;
;; In order for a better dictionary structure to help, env-compose would have to combine
;; sub-dictionaries.  But this is only possible if the sub-dictionaries are immutable, which is not
;; the case for definition-style environments until they are frozen.
;;
;; We could have env-compose recognize frozen children and combine their dictionaries when possible.

(define (env-freeze env)
  (lambda (method)
    (case method
      ((describe) (list 'freeze (env 'describe)))
      ((ref)      (env 'ref))
      ((set!)     (lambda (fail id x) (error "invalid frozen environment operation" method id x)))
      (else       (error "invalid environment operation" method)))))

(define (env-compose env env.first)
  (lambda (method)
    (case method
      ((describe) (list 'compose (env.first 'describe) (env 'describe)))
      ((ref)      (lambda (fail id)   ((env.first 'ref)  (lambda () ((env 'ref)  fail id))   id)))
      ((set!)     (lambda (fail id x) ((env.first 'set!) (lambda () ((env 'set!) fail id x)) id x)))
      (else       (error "invalid environment operation" method)))))

(define (env-compose* env . env*)
  (let loop ((env env) (env* env*))
    (if (null? env*)
        env
        (env-compose env (loop (car env*) (cdr env*))))))

(define (env-mark env m)
  (define (unmark id) (and (identifier? id) (syntax-remove-mark? id m)))
  (lambda (method)
    (case method
      ((describe) (list 'mark m (env 'describe)))
      ((ref)      (lambda (fail id)   (let ((i (unmark id))) (if i ((env 'ref)  fail i)   (fail)))))
      ((set!)     (lambda (fail id x) (let ((i (unmark id))) (if i ((env 'set!) fail i x) (fail)))))
      (else       (error "invalid environment operation" method)))))

(splicing-local
  ;((splicing-local
  ;   ((define (id-dict-top-symbol id=>x) (vector-ref id=>x 0))
  ;    (define (id-dict-top-mark*  id=>x) (vector-ref id=>x 1))
  ;    (define (id-dict-top-value  id=>x) (vector-ref id=>x 2))
  ;    (define (id-dict-pop        id=>x) (vector-ref id=>x 3)))
  ;   (define id-dict.empty '())
  ;   (define (id-dict-empty? id=>x) (null? id=>x))
  ;   (define (id-dict-keys id=>x)
  ;     (let loop ((id=>x id=>x))
  ;       (cond ((id-dict-empty? id=>x) '())
  ;             (else (cons (syntax-wrap (id-dict-top-symbol id=>x) (id-dict-top-mark* id=>x))
  ;                         (loop (id-dict-pop id=>x)))))))
  ;   (define (id-dict-set id=>x id x) (vector (syntax-peek id) (syntax-mark* id) x id=>x))
  ;   (define (id-dict-ref id=>x id)
  ;     (identifier?! id)
  ;     (let ((sym (syntax-peek id)) (m* (syntax-mark* id)))
  ;       (let loop ((id=>x id=>x))
  ;         (and (not (id-dict-empty? id=>x))
  ;              (cond ((and (eq? (id-dict-top-symbol id=>x) sym)
  ;                          (mark*=? (id-dict-top-mark* id=>x) m*))
  ;                     (id-dict-top-value id=>x))
  ;                    (else (loop (id-dict-pop id=>x))))))))))

  ((splicing-local
     ((define (alist-ref    kv* k)   (let ((kv (assq k kv*))) (and kv (cdr kv))))
      (define (alist-set    kv* k v) (cons (cons k v) (alist-remove kv* k)))
      (define (alist-remove kv* k)
        (let ((kv (assq k kv*)))
          (if kv
              (let loop ((kv* kv*))
                (cond ((eq? (car kv*) kv) (cdr kv*))
                      (else               (cons (car kv*) (loop (cdr kv*))))))
              kv*)))
      (define trie.empty '(()))
      (define (trie-ref t m* sym)
        (let loop ((t t) (m* m*))
          (if (null? m*)
              (alist-ref (car t) sym)
              (let ((t.next (alist-ref (cdr t) (car m*))))
                (and t.next (loop t.next (cdr m*)))))))
      (define (trie-set t m* sym x)
        (let loop ((t t) (m* m*))
          (if (null? m*)
              (cons (alist-set (car t) sym x) (cdr t))
              (cons (car t)
                    (alist-set (cdr t) (car m*)
                               (loop (or (alist-ref (cdr t) (car m*)) trie.empty) (cdr m*))))))))
     (define id-dict.empty trie.empty)
     (define (id-dict-key* id=>x)
       (let loop ((t id=>x))
         (apply append (map car (car t))
                (map (lambda (kv)
                       (let ((m (car kv)))
                         (map (lambda (id) (syntax-add-mark id m))
                              (map loop (cdr kv)))))
                     (cdr t)))))
     (define (id-dict-ref id=>x id)
       (identifier?! id)
       (trie-ref id=>x (syntax-mark* id) (syntax-peek id)))
     (define (id-dict-set id=>x id x)
       (identifier?! id)
       (trie-set id=>x (syntax-mark* id) (syntax-peek id) x))))

  (define (make-env)
    (let ((&id=>x (box id-dict.empty)))
      (lambda (method)
        (case method
          ((describe) (list->vector (id-dict-key* (unbox &id=>x))))
          ((ref)      (lambda (fail id) (or (id-dict-ref (unbox &id=>x) id) (fail))))
          ((set!)     (lambda (fail id x) (set-box! &id=>x (id-dict-set (unbox &id=>x) id x))))
          (else       (error "invalid environment operation" method)))))))

(define (env-describe env)      (env 'describe))
(define (env-set!     env id x) ((env 'set!) (lambda () (error "cannot env-set!" id)) id x))
(define (env-ref      env id)   ((env 'ref) (lambda () #f) id))

(define env.empty (let ((env (make-env))) (env-freeze env)))
