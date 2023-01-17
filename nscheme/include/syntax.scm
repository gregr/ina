;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

   (define (annotated-syntax form provenance)
     (if (not provenance)
         form
         (svector 'annotated-syntax form provenance)))

   (define (marked-syntax mark* stx)
     (if (null? mark*)
         stx
         (svector 'marked-syntax mark* stx)))

   (define (annotated-syntax? x) (and (svector? x)
                                      (= (svector-length x) 3)
                                      (eq? (svector-ref x 0) 'annotated-syntax)))
   (define (marked-syntax?    x) (and (svector? x)
                                      (= (svector-length x) 3)
                                      (eq? (svector-ref x 0) 'marked-syntax)))

   (define (marked-syntax-mark*               s) (svector-ref s 1))
   (define (marked-syntax-syntax              s) (svector-ref s 2))
   (define (maybe-annotated-syntax-form       s) (if (annotated-syntax? s) (svector-ref s 1) s))
   (define (maybe-annotated-syntax-provenance s) (if (annotated-syntax? s) (svector-ref s 2) s)))

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

(define (syntax->list s) (or (syntax->list? s) (raise-syntax-error "not a list" s)))

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
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-add-mark env m)
  (lambda (method)
    (case method
      ((address)    (lambda (k id) (let ((i (and (identifier? id) (syntax-remove-mark? id m))))
                                     (if i ((env 'address) k i) (k id)))))
      ((ref)        (env 'ref))
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
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-address env id)           ((env 'address) (lambda (id) #f) id))
(define (env-ref     env vocab addr)   ((env 'ref)     (lambda (vocab addr) #f) vocab addr))
(define (env-ref^    env vocab id)     (let ((addr (env-address env id)))
                                         (and addr (env-ref env vocab addr))))
(define (env-bind!   env id addr)      ((env 'bind!)   id addr))
(define (env-set!    env vocab addr v) ((env 'set!)    vocab addr v))

(define (transcribe env.op op env.use stx)
  (let* ((result (op (syntax-add-mark stx antimark)))
         (m      (fresh-mark))
         (env    (env-extend (env-add-mark env.op m) env.use))
         (stx    (syntax-provenance-add
                   (if (procedure? result)
                       (let* ((lookup    (lambda (vocab id)
                                           (env-ref^ env vocab (syntax-add-mark id m))))
                              (free-id=? (lambda (a b)
                                           (free-identifier=?/env
                                             env (syntax-add-mark a m) (syntax-add-mark b m)))))
                         (result lookup free-id=?))
                       result)
                   stx)))
    (values env (syntax-add-mark stx m))))
