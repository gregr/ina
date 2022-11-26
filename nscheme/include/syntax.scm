;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark-based renaming and qualified identifiers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (mvector))
(define mark=?       equal?)
(define mark*=?      equal?)
(define antimark     #f)

(splicing-local
  ((define antimark? not)
   (define (marks-append m*.outer m*.inner)
     (cond ((null? m*.inner)                 m*.outer)
           ((null? m*.outer)                 m*.inner)
           ((not (antimark? (car m*.inner))) (append m*.outer m*.inner))
           (else (let loop ((m (car m*.outer)) (m* (cdr m*.outer)))
                   (cond ((null? m*) (cdr m*.inner))
                         (else       (cons m (loop (car m*) (cdr m*)))))))))

   (define (make-qualified         x q) (svector 'qualified x q))
   (define (qualified-datum        q)   (svector-ref q 1))
   (define (qualified-vocab=>value q)   (svector-ref q 2))
   (define (qualified?             x)   (and (svector? x)
                                             (= (svector-length x) 3)
                                             (eq? (svector-ref x 0) 'qualified)))

   (define (make-syntax mark* datum provenance)
     (if (and (null? mark*) (not provenance))
         datum
         (svector 'syntax mark* datum provenance)))

   (define (syntax-peek0 s) (if (syntax-wrapped? s) (svector-ref s 2) s)))

  (define (syntax-wrapped?       x)    (and (svector? x)
                                            (= (svector-length x) 4)
                                            (eq? (svector-ref x 0) 'syntax)))
  (define (syntax-mark*          s)    (if  (syntax-wrapped? s) (svector-ref s 1) '()))
  (define (syntax-provenance     s)    (and (syntax-wrapped? s) (svector-ref s 3)))
  (define (syntax-provenance-set s pv) (if pv (make-syntax (syntax-mark* s) (syntax-peek0 s) pv) s))
  (define (syntax-provenance-add s pv) (syntax-provenance-set s (provenance-combine
                                                                  pv (syntax-provenance s))))
  (define (syntax-peek           s)    (let ((d (syntax-peek0 s)))
                                         (if (qualified? d) (qualified-datum d) d)))

  (define (syntax-wrap s m*)
    (if (syntax-wrapped? s)
        (make-syntax (marks-append m* (syntax-mark* s))
                     (syntax-peek0 s)
                     (syntax-provenance s))
        (make-syntax m* s #f)))

  (define (syntax-unwrap s)
    (if (syntax-wrapped? s)
        (let ((d (syntax-peek s)) (m* (syntax-mark* s)))
          (define (wrap x) (if (syntax-wrapped? x) (syntax-wrap x m*) (make-syntax m* x #f)))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        s))

  (define (syntax-remove-mark? s m)
    (let ((m* (syntax-mark* s)))
      (and (pair? m*)
           (mark=? (car m*) m)
           (make-syntax (cdr m*) (syntax-peek0 s) (syntax-provenance s)))))

  (define (identifier?  s) (symbol? (syntax-peek s)))
  (define (identifier?! s) (has-type?! identifier? 'identifier? s))

  (define (identifier-qualifier id)
    (identifier?! id)
    (let ((x (syntax-peek0 id))) (if (qualified? x) (qualified-vocab=>value x) qualifier.empty)))

  (define (identifier-unqualify id) (identifier-qualify id qualifier.empty))

  (define (identifier-qualify id q)
    (define (wrap x) (make-syntax (syntax-mark* id) x (syntax-provenance id)))
    (cond ((not (qualifier-empty? q))                   (wrap (make-qualified (syntax-peek id) q)))
          ((qualifier-empty? (identifier-qualifier id)) id)
          (else                                         (wrap (syntax-peek id)))))

(define (syntax-add-mark s m) (syntax-wrap s (list m)))

(define (free-identifier=?/env env a b)
  (identifier?! a) (identifier?! b)
  (let ((addr.a (env-address env a)) (addr.b (env-address env b)))
    (if (or addr.a addr.b)
        (addr=? addr.a addr.b)
        (eq? (syntax-peek a) (syntax-peek b)))))

(define (bound-identifier=? a b)
  (identifier?! a) (identifier?! b)
  (and (eq? (syntax-peek a) (syntax-peek b))
       (mark*=? (syntax-mark* a) (syntax-mark* b))))

(define (bound-identifiers-unique? ids)
  (or (null? ids)
      (and (bound-identifiers-unique? (cdr ids))
           (let ((id.0 (car ids)))
             (not (memp (lambda (id) (bound-identifier=? id id.0)) (cdr ids)))))))

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

(define (syntax->list s) (or (syntax->list? s) (raise-syntax-error "not a list" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environments with vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-address description) (mvector description))
(define addr=? equal?)

(define (env-extend env env.first)
  (lambda (method)
    (case method
      ((address)    (lambda (k id)         ((env.first 'address)
                                            (lambda (id) ((env 'address) k id))
                                            id)))
      ((ref)        (lambda (k vocab addr) ((env.first 'ref)
                                            (lambda (vocab addr) ((env 'ref) k vocab addr))
                                            vocab addr)))
      ((qualify)    (lambda (k v* addr)    ((env.first 'qualify)
                                            (lambda (v* addr) ((env 'qualify) k v* addr))
                                            v* addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-add-mark env m)
  (lambda (method)
    (case method
      ((address)    (lambda (k id) (let ((i (and (identifier? id) (syntax-remove-mark? id m))))
                                     (if i ((env 'address) k i) (k id)))))
      ((ref)        (env 'ref))
      ((qualify)    (env 'qualify))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (make-env)
  (let ((id=>addr '()) (addr=>vocab=>value '()))
    (lambda (method)
      (case method
        ((address) (lambda (k id)
                     (let ((entry (assp (lambda (id.key) (bound-identifier=? id.key id)) id=>addr)))
                       (if entry (cdr entry) (k id)))))
        ((ref)     (lambda (k vocab addr)
                     (let ((entry (assoc addr addr=>vocab=>value)))
                       (if entry
                           (let ((entry (assoc vocab (cdr entry))))
                             (if entry (cdr entry) (k vocab addr)))
                           (k vocab addr)))))
        ((qualify) (lambda (k v*.seen addr)
                     (let ((entry (assoc addr addr=>vocab=>value)))
                       (if entry
                           (let loop ((v=>v (cdr entry)) (v* v*.seen))
                             (cond
                               ((null? v=>v)                 (k v* addr))
                               ((member (caar v=>v) v*.seen) (loop (cdr v=>v) v*))
                               (else                         (cons (car v=>v)
                                                                   (loop (cdr v=>v)
                                                                         (cons (caar v=>v) v*))))))
                           (k v*.seen addr)))))
        ((bind!)   (lambda (id addr) (set! id=>addr (cons (cons id addr) id=>addr))))
        ((set!)    (lambda (vocab addr value)
                     (unless addr (error "invalid environment address" vocab value))
                     (set! addr=>vocab=>value
                       (let ((entry.addr (assoc addr addr=>vocab=>value)))
                         (if entry.addr
                             (let* ((entry.vocab  (assoc vocab (cdr entry.addr)))
                                    (vocab=>value (cons (cons vocab value)
                                                        (if entry.vocab
                                                            (rem1p (lambda (entry)
                                                                     (equal? vocab (car entry)))
                                                                   (cdr entry.addr))
                                                            (cdr entry.addr)))))
                               (cons (cons addr vocab=>value)
                                     (rem1p (lambda (entry) (addr=? addr (car entry)))
                                            addr=>vocab=>value)))
                             (cons (cons addr (list (cons vocab value)))
                                   addr=>vocab=>value))))))
        (else      (error "invalid environment operation" method))))))

(define env.empty
  (lambda (method)
    (case method
      ((address)    (lambda (k id)         (k id)))
      ((ref)        (lambda (k vocab addr) (k vocab addr)))
      ((qualify)    (lambda (k v* addr)    (k v* addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-address env id)           ((env 'address) (lambda (id) #f) id))
(define (env-ref     env vocab addr)   ((env 'ref)     (lambda (vocab addr) #f) vocab addr))
(define (env-ref^    env vocab id)     (or (let ((addr (env-address env id)))
                                             (and addr (env-ref env vocab addr)))
                                           (qualifier-ref (identifier-qualifier id) vocab)))
(define (env-bind!   env id addr)      ((env 'bind!)   (identifier-unqualify id) addr))
(define (env-set!    env vocab addr v) ((env 'set!)    vocab addr v))

(define (syntax-qualify s env)
  (define (antimarked? s) (let ((m* (syntax-mark* s))) (and (pair? m*) (not (car m*)))))
  (let loop ((s s))
    (cond ((antimarked? s) s)
          ((identifier? s) (let ((addr (env-address env s)))
                             (if addr
                                 (identifier-qualify
                                   s ((env 'qualify) (lambda (v* a) '()) '() addr))
                                 s)))
          (else (let ((pv (syntax-provenance s)) (x (syntax-unwrap s)))
                  (syntax-provenance-set
                    (cond ((pair?   x) (cons (loop (car x)) (loop (cdr x))))
                          ((vector? x) (vector-map loop x))
                          (else        s))
                    pv))))))

(define (syntax-unqualify s) (syntax-qualify s env.empty))

(define qualifier.empty '())
(define qualifier-empty? null?)

(define (qualifier-ref q vocab) (and q (let ((entry (assoc vocab q))) (and entry (cdr entry)))))

(define (transcribe env.op op env.use stx)
  (let* ((result (op (syntax-add-mark stx antimark)))
         (m      (fresh-mark))
         (stx    (syntax-provenance-add
                   (if (procedure? result)
                       (let* ((env       (env-extend (env-add-mark env.op m) env.use))
                              (lookup    (lambda (vocab id)
                                           (env-ref^ env vocab (syntax-add-mark id m))))
                              (free-id=? (lambda (a b)
                                           (free-identifier=?/env
                                             env (syntax-add-mark a m) (syntax-add-mark b m)))))
                         (result lookup free-id=?))
                       result)
                   stx)))
    (syntax-add-mark (syntax-qualify stx env.op) m)))
