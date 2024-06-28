;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (mvector))

(splicing-local
  ((define antimark #f)
   (define mark=?   eq?)
   (define (mark*=? a* b*)
     (let loop ((a* a*) (b* b*))
       (if (pair? a*)
           (and (pair? b*)
                (mark=? (car a*) (car b*))
                (loop (cdr a*) (cdr b*)))
           (null? b*))))
   (define antimark? not)
   (define (marks-append m*.outer m*.inner)
     (cond ((null? m*.inner)                 m*.outer)
           ((null? m*.outer)                 m*.inner)
           ((not (antimark? (car m*.inner))) (append m*.outer m*.inner))
           (else (let loop ((m (car m*.outer)) (m* (cdr m*.outer)))
                   (cond ((null? m*) (cdr m*.inner))
                         (else       (cons m (loop (car m*) (cdr m*)))))))))

   (define rtd.annotated  (make-rtd 'syntax:annotated #f #t '#(form provenance) #f))
   (define annotated?     (record-predicate rtd.annotated))
   (define ann-form       (record-field-name-accessor rtd.annotated 'form))
   (define ann-provenance (record-field-name-accessor rtd.annotated 'provenance))
   (define (annotated form pv)      ((record-constructor rtd.annotated) form pv))
   (define (annotated-form       s) (if (annotated? s) (ann-form s) s))
   (define (annotated-provenance s) (and (annotated? s) (ann-provenance s)))

   (define rtd.marked   (make-rtd 'syntax:marked #f #t '#(mark* form) #f))
   (define marked?      (record-predicate rtd.marked))
   (define marked-mark* (record-field-name-accessor rtd.marked 'mark*))
   (define marked-form  (record-field-name-accessor rtd.marked 'form))
   (define (marked mark* form) (if (null? mark*) form ((record-constructor rtd.marked) mark* form)))

   (define (syntax-mark* s) (if (marked? s) (marked-mark* s) '()))
   (define (syntax-peek  s) (annotated-form (if (marked? s) (marked-form s) s)))
   (define (syntax-wrap s m*)
     (if (marked? s)
         (marked (marks-append m* (syntax-mark* s)) (marked-form s))
         (marked m* s)))
   (define (syntax-add-mark s m) (syntax-wrap s (list m)))
   (define (identifier-remove-mark id m)
     (identifier?! id)
     (let ((m* (syntax-mark* id)))
       (and (pair? m*)
            (mark=? (car m*) m)
            (marked (cdr m*) (marked-form id))))))

  (define (syntax-provenance s) (annotated-provenance (if (marked? s) (marked-form s) s)))
  (define (syntax-provenance-set s pv)
    (if (eq? pv (syntax-provenance s))
        s
        (marked (syntax-mark* s) (if pv (annotated (syntax-peek s) pv) (syntax-peek s)))))
  (define (syntax-provenance-add s pv)
    (if pv
        (let ((pv2 (syntax-provenance s)))
          (if (eq? pv pv2)
              s
              (marked (syntax-mark* s) (annotated (syntax-peek s) (if pv2 (cons pv pv2) pv)))))
        s))

  (define (syntax-unwrap s)
    (if (marked? s)
        (let ((d (syntax-peek s)) (m* (syntax-mark* s)))
          (define (wrap x) (syntax-wrap x m*))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        (annotated-form s)))

  (define (datum->syntax context datum)
    (identifier?! context)
    (syntax-wrap datum (syntax-mark* context)))

  (define (syntax->datum s)
    (let loop ((s s))
      (let ((x (syntax-peek s)))
        (cond ((pair?   x) (cons (loop (car x)) (loop (cdr x))))
              ((vector? x) (vector-map loop x))
              (else        x)))))

  (define (identifier?  s) (symbol? (syntax-peek s)))
  (define (identifier?! s) (unless (identifier? s) (error "not an identifier" s)))

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

  (define (transcribe op m env stx)
    (let ((result (op (syntax-add-mark stx antimark))))
      (syntax-add-mark
        (syntax-provenance-add
          (if (procedure? result) (result (env-unmark env m)) result)
          (syntax-provenance stx))
        m)))

  ;;;;;;;;;;;;;;;;;;;;
  ;;; Environments ;;;
  ;;;;;;;;;;;;;;;;;;;;
  ;; NOTE: lookup is currently O(n^2), but may not be a problem in practice.  Consider more efficient
  ;; dictionary data structures If we need to harden this.
  ;;
  ;; In order for a better dictionary structure to help, env-conjoin would have to combine
  ;; sub-dictionaries.  But this is only possible if the sub-dictionaries are immutable, which is not
  ;; the case for definition-style environments until they are frozen.
  ;;
  ;; We could have env-conjoin recognize frozen children and combine their dictionaries when possible.

  (define make-env
    (local
      ((define (alist-ref    kv* k)   (let ((kv (assq k kv*))) (and kv (cdr kv))))
       (define (alist-set    kv* k v) (cons (cons k v) (alist-remove kv* k)))
       (define (alist-remove kv* k)
         (let ((kv (assq k kv*)))
           (if kv
               (let loop ((kv* kv*))
                 (cond ((eq? (car kv*) kv) (cdr kv*))
                       (else               (cons (car kv*) (loop (cdr kv*))))))
               kv*)))
       (define trie.empty '(() . ()))  ; trie : `(,sym=>x . ,mark=>trie)
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
                                (loop (or (alist-ref (cdr t) (car m*)) trie.empty) (cdr m*)))))))
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
         (trie-set id=>x (syntax-mark* id) (syntax-peek id) x)))
      (lambda ()
        (mlet ((id=>x id-dict.empty))
          (lambda (method)
            (case method
              ((describe) (list->vector (id-dict-key* id=>x)))
              ((ref)      (lambda (fail id) (or (id-dict-ref id=>x id) (fail))))
              ((set!)     (lambda (id x) (set! id=>x (id-dict-set id=>x id x))))
              (else       (error "invalid environment operation" method))))))))

  (define (env-freeze env)
    (lambda (method)
      (case method
        ((describe) (list 'freeze (env 'describe)))
        ((ref)      (env 'ref))
        ((set!)     (lambda (id x) (error "set! with frozen environment" id x)))
        (else       (error "invalid environment operation" method)))))

  (define (env-unmark env.m m)
    (lambda (method)
      (case method
        ((describe) (list 'unmark m (env.m 'describe)))
        ((ref)      (lambda (fail id) ((env.m 'ref) fail (syntax-add-mark id m))))
        ((set!)     (lambda (id x) ((env.m 'set!) (syntax-add-mark id m) x)))
        (else       (error "invalid environment operation" method)))))

  (define (env-disjoin env.mark m env.no-mark)
    (lambda (method)
      (case method
        ((describe) (list 'disjoin (env.mark 'describe) m (env.no-mark 'describe)))
        ((ref)      (lambda (fail id)
                      (let ((i (identifier-remove-mark id m)))
                        (if i ((env.mark 'ref) fail i) ((env.no-mark 'ref) fail id)))))
        ((set!)     (lambda (id x)
                      (let ((i (identifier-remove-mark id m)))
                        (if i ((env.mark 'set!) i x) ((env.no-mark 'set!) id x)))))
        (else       (error "invalid environment operation" method)))))

  (define (env-conjoin env.first env.second)
    (lambda (method)
      (case method
        ((describe) (list 'conjoin (env.first 'describe) (env.second 'describe)))
        ((ref)      (lambda (fail id)
                      ((env.first 'ref) (lambda () ((env.second 'ref) fail id)) id)))
        ((set!)     (lambda (id x) ((env.first 'set!) id x)))
        (else       (error "invalid environment operation" method)))))

  (define (env-conjoin* env.first . env*.rest)
    (let loop ((env env.first) (env* env*.rest))
      (if (null? env*)
          env
          (env-conjoin env (loop (car env*) (cdr env*))))))

  (define (env-describe env)      (env 'describe))
  (define (env-ref      env id)   ((env 'ref) (lambda () #f) id))
  (define (env-set!     env id x) ((env 'set!) id x))

  (define package.syntax
    (cons
      '(
        syntax-provenance syntax-provenance-set syntax-provenance-add
        syntax-unwrap syntax->datum datum->syntax fresh-mark transcribe
        identifier? identifier?! bound-identifier=? free-identifier=?/env
        make-env env-freeze env-disjoin env-conjoin env-conjoin* env-describe env-ref env-set!)
      (list
        syntax-provenance syntax-provenance-set syntax-provenance-add
        syntax-unwrap syntax->datum datum->syntax fresh-mark transcribe
        identifier? identifier?! bound-identifier=? free-identifier=?/env
        make-env env-freeze env-disjoin env-conjoin env-conjoin* env-describe env-ref env-set!))))
