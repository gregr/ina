;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-mark-level (make-parameter 0))

(splicing-local
  ((define-values (unused-subtype-mark make-mark mark? access-mark unused-mutate-mark!)
     (make-record-type 'mark 1 '() #t #f #f))
   (define (mark-level m) (access-mark m 0))
   (define mark=? eqv?)
   (define (mark*=? a* b*)
     (let loop ((a* a*) (b* b*))
       (if (pair? a*)
           (and (pair? b*)
                (mark=? (car a*) (car b*))
                (loop (cdr a*) (cdr b*)))
           (null? b*))))
   (define (mark*-append m*.outer m*.inner)
     (cond ((null? m*.inner) m*.outer)
           ((null? m*.outer) m*.inner)
           (else (let loop ((m*.outer (reverse m*.outer)) (m*.inner m*.inner))
                   (if (mark=? (car m*.outer) (car m*.inner))
                       (let ((m*.outer (cdr m*.outer)) (m*.inner (cdr m*.inner)))
                         (cond ((null? m*.outer) m*.inner)
                               ((null? m*.inner) (reverse m*.outer))
                               (else             (loop m*.outer m*.inner))))
                       (cons (car m*.outer)
                             (let loop ((m*.outer (cdr m*.outer)) (m* m*.inner))
                               (if (null? m*.outer)
                                   m*
                                   (loop (cdr m*.outer) (cons (car m*.outer) m*))))))))))
   (define-values (unused-subtype-annotated make-annotated annotated? access-annotated unused-mutate-annotated!)
     (make-record-type 'syntax:annotated 2 #f #t #f #f))
   (define (annotated form note) (if (null? note) form (make-annotated form note)))
   (define (annotated-form s) (if (annotated? s) (access-annotated s 0) s))
   (define (annotated-note s) (if (annotated? s) (access-annotated s 1) '()))
   (define-values (unused-subtype-marked make-marked marked? access-marked unused-mutate-marked!)
     (make-record-type 'syntax:marked 2 #f #t #f #f))
   (define (marked mark* form) (if (null? mark*) form (make-marked mark* form)))
   (define (marked-form  s) (access-marked s 1))
   (define (syntax-form  s) (annotated-form (if (marked? s) (marked-form s) s)))
   (define (syntax-mark* s) (if (marked? s) (access-marked s 0) '()))
   (define (syntax-wrap s m*)
     (if (marked? s)
         (marked (mark*-append m* (syntax-mark* s)) (marked-form s))
         (let ((form (annotated-form s)))
           (if (or (pair? form) (vector? form) (symbol? form)) (marked m* s) s))))
   (define (syntax-add-mark s m) (syntax-wrap s (list m)))
   (define (identifier-remove-mark id m)
     (identifier?! id)
     (let ((m* (syntax-mark* id)))
       (and (pair? m*) (mark=? (car m*) m) (marked (cdr m*) (marked-form id))))))

  (define (fresh-mark) (make-mark (current-mark-level)))

  (define (syntax-note     s)       (annotated-note (if (marked? s) (marked-form s) s)))
  (define (syntax-note-set s note) (marked (syntax-mark* s) (annotated (syntax-form s) note)))
  (define (syntax-note-add s note) (syntax-note-set s (atree-replace (syntax-note s) note)))

  (define (syntax-unwrap s)
    (if (marked? s)
        (let ((d (syntax-form s)) (m* (syntax-mark* s)))
          (define (wrap x) (syntax-wrap x m*))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        (annotated-form s)))

  (define (syntax-prune-level s level)
    (let prune ((s s))
      (let loop ((m* (syntax-mark* s)))
        (if (null? m*)
            (let ((d (syntax-form s)))
              (cond ((pair?   d) (cons (prune (car d)) (prune (cdr d))))
                    ((vector? d) (vector-map prune d))
                    (else        d)))
            (let ((m (car m*)))
              (if (<= level (mark-level m))
                  (loop (cdr m*))
                  (marked m* (marked-form s))))))))

  (define (datum->syntax context datum)
    (identifier?! context)
    (if (marked? context)
        (syntax-wrap datum (syntax-mark* context))
        datum))

  (define (syntax->datum s)
    (let loop ((s s))
      (let ((x (syntax-form s)))
        (cond ((pair?   x) (cons (loop (car x)) (loop (cdr x))))
              ((vector? x) (vector-map loop x))
              (else        x)))))

  (define (identifier?  s) (symbol? (syntax-form s)))
  (define (identifier?! s) (unless (identifier? s) (mistake "not an identifier" s)))
  (define (identifier=? a b)
    (identifier?! a) (identifier?! b)
    (and (eqv? (syntax-form a) (syntax-form b))
         (mark*=? (syntax-mark* a) (syntax-mark* b))))

  (define (transcribe op m env stx)
    (let ((result (op (syntax-add-mark stx m))))
      (syntax-add-mark (if (procedure? result) (result (env-unmark env m)) result) m)))

  ;;;;;;;;;;;;;;;;;;;;
  ;;; Environments ;;;
  ;;;;;;;;;;;;;;;;;;;;
  ;; NOTE: lookup is currently O(n^2), but may not be a problem in practice.  Consider more efficient
  ;; dictionary data structures If we need to harden this.
  ;;
  ;; In order for a better dictionary structure to help, env-conjoin would have to combine
  ;; sub-dictionaries.  But this is only possible if the sub-dictionaries are immutable, which is not
  ;; the case for definition-style environments until they are read-only.
  ;;
  ;; We could have env-conjoin recognize read-only children and combine their dictionaries when possible.

  (define make-env
    (local
      ((define (alist-ref    kv* k)   (let ((kv (assv k kv*))) (and kv (cdr kv))))
       (define (alist-set    kv* k v) (cons (cons k v) (alist-remove kv* k)))
       (define (alist-remove kv* k)
         (let ((kv (assv k kv*)))
           (if kv
               (let loop ((kv* kv*))
                 (cond ((eqv? (car kv*) kv) (cdr kv*))  ; assumes mark=? is eqv?
                       (else                (cons (car kv*) (loop (cdr kv*))))))
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
                     (alist-set (cdr t) (car m*)  ; requires mark=? assumption in alist-remove
                                (loop (or (alist-ref (cdr t) (car m*)) trie.empty) (cdr m*)))))))
       (define id-dict.empty trie.empty)
       (define (id-dict-key* id=>x)
         (let loop ((t id=>x))
           (append (map car (car t))
                   (append* (map (lambda (kv)
                                   (let ((m (car kv)))
                                     (map (lambda (id) (syntax-add-mark id m))
                                          (map loop (cdr kv)))))
                                 (cdr t))))))
       (define (id-dict-ref id=>x id)
         (identifier?! id)
         (trie-ref id=>x (syntax-mark* id) (syntax-form id)))
       (define (id-dict-set id=>x id x)
         (identifier?! id)
         (trie-set id=>x (syntax-mark* id) (syntax-form id) x)))
      (lambda ()
        (mlet ((id=>x id-dict.empty))
          (lambda (method)
            (case method
              ((describe) (list->vector (id-dict-key* id=>x)))
              ((ref)      (lambda (fail id) (or (id-dict-ref id=>x id) (fail))))
              ((set!)     (lambda (id x) (set! id=>x (id-dict-set id=>x id x))))
              (else       (mistake "invalid environment operation" method))))))))

  (define (env-read-only env)
    (lambda (method)
      (case method
        ((describe) (list 'read-only (env 'describe)))
        ((ref)      (env 'ref))
        ((set!)     (lambda (id x) (mistake 'env-set! "read-only environment" id x)))
        (else       (mistake "invalid environment operation" method)))))

  (define (env-unmark env.m m)
    (lambda (method)
      (case method
        ((describe) (list 'unmark m (env.m 'describe)))
        ((ref)      (lambda (fail id) ((env.m 'ref) fail (syntax-add-mark id m))))
        ((set!)     (lambda (id x) ((env.m 'set!) (syntax-add-mark id m) x)))
        (else       (mistake "invalid environment operation" method)))))

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
        (else       (mistake "invalid environment operation" method)))))

  (define (env-conjoin env.first env.second)
    (lambda (method)
      (case method
        ((describe) (list 'conjoin (env.first 'describe) (env.second 'describe)))
        ((ref)      (lambda (fail id)
                      ((env.first 'ref) (lambda () ((env.second 'ref) fail id)) id)))
        ((set!)     (lambda (id x) ((env.first 'set!) id x)))
        (else       (mistake "invalid environment operation" method)))))

  (define (env-conjoin* env.first . env*.rest)
    (let loop ((env env.first) (env* env*.rest))
      (if (null? env*)
          env
          (env-conjoin env (loop (car env*) (cdr env*))))))

  (define (env-describe env)      (env 'describe))
  (define (env-ref      env id)   ((env 'ref) (lambda () #f) id))
  (define (env-set!     env id x) ((env 'set!) id x)))
