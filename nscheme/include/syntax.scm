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

   (define rtd.annotated-syntax (make-rtd 'annotated-syntax #f #t '#(form provenance)))
   (define (make-annotated-syntax form provenance)
     ((record-constructor rtd.annotated-syntax) form provenance))
   (define (annotated-syntax form provenance)
     (if (not provenance) form (make-annotated-syntax form provenance)))
   (define annotated-syntax?           (record-predicate rtd.annotated-syntax))
   (define annotated-syntax-form       (record-field-position-accessor rtd.annotated-syntax 0))
   (define annotated-syntax-provenance (record-field-position-accessor rtd.annotated-syntax 1))
   (define (maybe-annotated-syntax-form s) (if (annotated-syntax? s) (annotated-syntax-form s) s))
   (define (maybe-annotated-syntax-provenance s) (and (annotated-syntax? s)
                                                      (annotated-syntax-provenance s)))

   (define rtd.marked-syntax (make-rtd 'marked-syntax #f #t '#(mark* syntax)))
   (define (make-marked-syntax mark* syntax) ((record-constructor rtd.marked-syntax) mark* syntax))
   (define (marked-syntax      mark* stx)    (if (null? mark*) stx (make-marked-syntax mark* stx)))
   (define marked-syntax?       (record-predicate rtd.marked-syntax))
   (define marked-syntax-mark*  (record-field-position-accessor rtd.marked-syntax 0))
   (define marked-syntax-syntax (record-field-position-accessor rtd.marked-syntax 1)))

  (define (syntax-mark*          s)    (if (marked-syntax? s) (marked-syntax-mark* s) '()))
  (define (syntax-peek           s)    (maybe-annotated-syntax-form
                                         (if (marked-syntax? s) (marked-syntax-syntax s) s)))
  (define (syntax-provenance     s)    (maybe-annotated-syntax-provenance
                                         (if (marked-syntax? s) (marked-syntax-syntax s) s)))
  (define (syntax-provenance-set s pv) (if pv
                                           (marked-syntax (syntax-mark* s)
                                                          (annotated-syntax (syntax-peek s) pv))
                                           s))
  (define (syntax-provenance-add s pv) (syntax-provenance-set s (provenance-combine
                                                                  pv (syntax-provenance s))))

  (define (syntax-wrap s m*)
    (if (marked-syntax? s)
        (marked-syntax (marks-append m* (syntax-mark* s)) (marked-syntax-syntax s))
        (marked-syntax m* s)))

  (define (syntax-unwrap s)
    (if (marked-syntax? s)
        (let ((d (syntax-peek s)) (m* (syntax-mark* s)))
          (define (wrap x) (syntax-wrap x m*))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        (maybe-annotated-syntax-form s)))

  (define (syntax-remove-mark? s m)
    (let ((m* (syntax-mark* s)))
      (and (pair? m*)
           (mark=? (car m*) m)
           (marked-syntax (cdr m*) (marked-syntax-syntax s))))))

(define (identifier?  s) (symbol? (syntax-peek s)))
(define (identifier?! s) (has-type?! identifier? 'identifier? s))

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

(define (syntax->list s) (or (syntax->list? s) (raise-syntax-error "not a list" s)))

;;;;;;;;;;;;;;;;;;;;
;;; Environments ;;;
;;;;;;;;;;;;;;;;;;;;

;; NOTE: lookup is currently O(n^2), but may not be a problem in practice.  Consider more efficient
;; dictionary data structures If we need to harden this.
;;
;; In order for a better dictionary structure to help, env-extend would have to combine
;; sub-dictionaries.  But this is only possible if the sub-dictionaries are immutable, which is not
;; the case for definition-style environments until they are frozen.
;;
;; We could have env-extend listen for freeze! events coming from its children.  Once both children
;; are frozen, we would combine their dictionaries, then propagate our own freeze! event to any
;; further registered extensions.

(define (env-extend env env.first)
  (lambda (method)
    (caseq method
      ((describe) (list 'extend (env.first 'describe) (env 'describe)))
      ((ref)      (lambda (fail id)   ((env.first 'ref)  (lambda () ((env 'ref)  fail id))   id)))
      ((set!)     (lambda (fail id x) ((env.first 'set!) (lambda () ((env 'set!) fail id x)) id x)))
      ((freeze!)  (values))
      (else       (error "invalid environment operation" method)))))

(define (env-mark env m)
  (define (unmark id) (and (identifier? id) (syntax-remove-mark? id m)))
  (lambda (method)
    (caseq method
      ((describe) (list 'mark m (env 'describe)))
      ((ref)      (lambda (fail id)   (let ((i (unmark id))) (if i ((env 'ref)  fail i)   (fail)))))
      ((set!)     (lambda (fail id x) (let ((i (unmark id))) (if i ((env 'set!) fail i x) (fail)))))
      ((freeze!)  (values))
      (else       (error "invalid environment operation" method)))))

(splicing-local
  ((define sym-dict.empty '())
   (define (sym-dict-keys sym=>x) (map car sym=>x))
   (define (sym-dict-set sym=>x sym x) (has-type?! symbol? 'symbol? sym) (cons (cons sym x) sym=>x))
   (define (sym-dict-ref sym=>x sym)
     (has-type?! symbol? 'symbol? sym)
     (let ((kv (assq sym sym=>x))) (and kv (cdr kv)))))
  (define (make-env)
    (let ((&sym=>x (mvector sym-dict.empty)) (mv.frozen? (mvector #f)))
      (define (frozen?) (mvector-ref mv.frozen? 0))
      (define (^sym=>x) (mvector-ref &sym=>x 0))
      (lambda (method)
        (caseq method
          ((describe) (let ((frame (list->vector (sym-dict-keys (^sym=>x)))))
                        (if (frozen?) frame (cons 'incomplete frame))))
          ((ref)      (lambda (fail id)
                        (if (null? (syntax-mark* id))
                            (or (sym-dict-ref (^sym=>x) (syntax-peek id)) (fail))
                            (fail))))
          ((set!)     (lambda (fail id x)
                        ;; We do not want to invoke the failure continuation since it could lead to
                        ;; unintended set! behavior in composed environments.
                        (when (frozen?) (error "invalid immutable environment operation" method id))
                        (if (null? (syntax-mark* id))
                            (mvector-set! &sym=>x 0 (sym-dict-set (^sym=>x) (syntax-peek id) x))
                            (error "cannot set! marked identifier in unmarked environment" id))))
          ((freeze!)  (mvector-set! mv.frozen? 0 #t))
          (else       (error "invalid environment operation" method)))))))

(define (env-describe env)      (env 'describe))
(define (env-freeze!  env)      (env 'freeze!))
(define (env-set!     env id x) ((env 'set!) (lambda () (error "cannot env-set!" id)) id x))
(define (env-ref      env id)   ((env 'ref) (lambda () #f) id))

(define env.empty (let ((env (make-env))) (env-freeze! env) env))
