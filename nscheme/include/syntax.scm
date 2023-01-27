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

   (define rtd.annotated-syntax (vector 2))
   (define rtd.marked-syntax    (vector 2))

   (define (annotated-syntax form provenance)
     (if (not provenance)
         form
         (let ((r (make-record rtd.annotated-syntax 0)))
           (record-set! r 0 form)
           (record-set! r 1 provenance)
           r)))

   (define (marked-syntax mark* stx)
     (if (null? mark*)
         stx
         (let ((r (make-record rtd.marked-syntax 0)))
           (record-set! r 0 mark*)
           (record-set! r 1 stx)
           r)))

   (define (annotated-syntax? x) (and (record? x) (eq? (record-type-descriptor x)
                                                       rtd.annotated-syntax)))
   (define (marked-syntax?    x) (and (record? x) (eq? (record-type-descriptor x)
                                                       rtd.marked-syntax)))

   (define (marked-syntax-mark*               s) (record-ref s 0))
   (define (marked-syntax-syntax              s) (record-ref s 1))
   (define (maybe-annotated-syntax-form       s) (if (annotated-syntax? s) (record-ref s 0) s))
   (define (maybe-annotated-syntax-provenance s) (if (annotated-syntax? s) (record-ref s 1) #f)))

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

(define env.empty
  (lambda (method)
    (caseq method
      ((ref)     (lambda (fail id) (fail)))
      ((freeze!) (lambda () (values)))
      ((set!)    (error "invalid immutable environment operation" method))
      (else      (error "invalid environment operation"           method)))))

(define (env-extend env env.first)
  (lambda (method)
    (caseq method
      ((ref)     (lambda (fail id) ((env.first 'ref) (lambda () ((env 'ref) fail id)) id)))
      ((freeze!) (lambda () (values)))
      ((set!)    (error "invalid immutable environment operation" method))
      (else      (error "invalid environment operation"           method)))))

(define (env-add-mark env m)
  (lambda (method)
    (caseq method
      ((ref)     (lambda (fail id) (let ((i (and (identifier? id) (syntax-remove-mark? id m))))
                                     (if i ((env 'ref) fail i) (fail id)))))
      ((freeze!) (lambda () (values)))
      ((set!)    (error "invalid immutable environment operation" method))
      (else      (error "invalid environment operation"           method)))))

(splicing-local
  ((define id=? bound-identifier=?)
   (define id-dict.empty '())
   (define (id-dict-ref id=>x id)
     (let ((kv (assp (lambda (id.key) (id=? id.key id)) id=>x)))
       (and kv (cdr kv))))
   (define (id-dict-set id=>x id x) (cons (cons id x) id=>x)))
  (define (make-env)
    (let ((id=>x (mvector id-dict.empty)) (frozen? (mvector #f)))
      (lambda (method)
        (caseq method
          ((ref)     (lambda (fail id) (or (id-dict-ref (mvector-ref id=>x 0) id) (fail))))
          ((freeze!) (lambda () (mvector-set! frozen? 0 #t)))
          ((set!)    (lambda (id x)
                       (when (mvector-ref frozen? 0)
                         (error "invalid immutable environment operation" method))
                       (mvector-set! id=>x 0 (id-dict-set (mvector-ref id=>x 0) id x))))
          (else      (error "invalid environment operation" method)))))))

(define (env-freeze! env)      ((env 'freeze!)))
(define (env-set!    env id x) ((env 'set!) id x))
(define (env-ref     env id)   ((env 'ref) (lambda () #f) id))
