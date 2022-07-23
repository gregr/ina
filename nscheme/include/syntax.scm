;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark-based renaming ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (mvector))
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

   (define (make-syntax marks datum provenance) (svector 'syntax marks datum provenance))

   (define (syntax-marks s) (if (syntax-wrapped? s) (svector-ref s 1) '()))
   (define (syntax-datum s) (if (syntax-wrapped? s) (svector-ref s 2) s))

   (define (syntax-mark* s m*)
     (if (syntax-wrapped? s)
         (make-syntax (marks-append m* (syntax-marks s))
                      (syntax-datum s)
                      (syntax-provenance s))
         (make-syntax m* s #f))))

  (define (syntax-wrapped? x) (and (svector? x)
                                   (= (svector-length x) 4)
                                   (eq? (svector-ref x 0) 'syntax)))

  (define (syntax-provenance     s)    (and (syntax-wrapped? s) (svector-ref s 3)))
  (define (syntax-provenance-set s pv) (if pv (make-syntax (syntax-marks s) (syntax-datum s) pv) s))
  (define (syntax-provenance-add s pv) (syntax-provenance-set s (provenance-combine
                                                                  pv (syntax-provenance s))))

  (define (identifier?  s) (and (syntax-wrapped? s) (symbol? (syntax-datum s))))
  (define (identifier?! s) (has-type?! identifier? 'identifier? s))

  (define (free-identifier=?  env a b)
    (identifier?! a) (identifier?! b)
    (let ((addr.a (env-address env a)) (addr.b (env-address env b)))
      (if (or addr.a addr.b)
        (eq? addr.a addr.b)
        (eq? (syntax-datum a) (syntax-datum b)))))

  (define (bound-identifier=? a b)
    (identifier?! a) (identifier?! b)
    (and (eq?    (syntax-datum a) (syntax-datum b))
         (equal? (syntax-marks a) (syntax-marks b))))

  (define (bound-identifiers-unique? ids)
    (or (null? ids)
        (and (bound-identifiers-unique? (cdr ids))
             (let ((id.0 (car ids)))
               (not (memp (lambda (id) (bound-identifier=? id id.0)) (cdr ids)))))))

  (define (datum->syntax context datum)
    (identifier?! context)
    (syntax-mark* datum (syntax-marks context)))

  (define (syntax->datum x)
    (let loop ((x x))
      (let strip ((d0 (if (syntax-wrapped? x) (syntax-datum x) x)))
        (cond ((pair?   d0) (cons (loop (car d0)) (loop (cdr d0))))
              ((vector? d0) (vector-map loop d0))
              (else         d0)))))

  (define (syntax-mark s m) (syntax-mark* s (list m)))

  (define (syntax-unmark s m)
    (and (syntax-wrapped? s)
         (let ((m* (syntax-marks s)))
           (and (pair? m*)
                (equal? (car m*) m)
                (make-syntax (cdr m*) (syntax-datum s) (syntax-provenance s))))))

  (define (syntax-unwrap s)
    (if (syntax-wrapped? s)
        (let ((d (syntax-datum s)) (m* (syntax-marks s)))
          (define (wrap x) (if (syntax-wrapped? x) (syntax-mark* x m*) (make-syntax m* x #f)))
          (cond ((pair?   d) (cons (wrap (car d)) (wrap (cdr d))))
                ((vector? d) (vector-map wrap d))
                (else        d)))
        s))

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

  ;; hygienic? should be used to ensure that no raw symbols or unmarked
  ;; identifiers appear anywhere in a hygienic transcription output.
  ;;
  ;; Raw symbols are dangerous to produce during hygienic transcription.  If the
  ;; definition of a syntax transformer that produces a raw symbol was itself
  ;; produced by a hygienic transcription, the raw symbol produced will not
  ;; retain the mark from that transcription which produced the definition.
  ;; This means a raw symbol will not reliably refer to an identifier bound in
  ;; the transformer definition's environment.
  ;;
  ;; Additionally, all intended identifiers, including those that have not been
  ;; produced during hygienic transcription, should include at least one mark.
  ;; Any unmarked identifier encountered is assumed to have been produced
  ;; inadvertently from a raw symbol, and should also be ruled out.
  (define (hygienic? x)
    (cond ((pair?           x) (and (hygienic? (car x)) (hygienic? (cdr x))))
          ((vector?         x) (hygienic? (vector->list x)))
          ((syntax-wrapped? x) (or (pair? (syntax-marks x)) (hygienic? (syntax-datum x))))
          (else                (not (symbol? x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments with vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-address description) (mvector description))

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

(define (env-mark env m)
  (lambda (method)
    (case method
      ((address)    (lambda (k id)         (let ((i (and (identifier? id) (syntax-unmark id m))))
                                             (if i ((env 'address) k i) (k id)))))
      ((ref)        (lambda (k vocab addr) (env-ref env vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (make-env)
  (let ((id=>addr (make-hash)) (addr=>vocab=>value (make-hash)))
    (lambda (method)
      (case method
        ((address) (lambda (k id)         (hash-ref id=>addr id (lambda () (k id)))))
        ((ref)     (lambda (k vocab addr) (hash-ref (hash-ref addr=>vocab=>value addr (hash))
                                                    vocab (lambda () (k vocab addr)))))
        ((bind!)   (lambda (id addr)      (hash-set! id=>addr id addr)))
        ((set!)    (lambda (vocab addr value)
                     (unless addr (error "invalid environment address" addr))
                     (hash-update!
                       addr=>vocab=>value addr
                       (lambda (vocab=>value) (hash-set vocab=>value vocab value))
                       (hash))))
        (else      (error "invalid environment operation" method))))))

(define env.empty
  (lambda (method)
    (case method
      ((address)    (lambda (k id)         (syntax->datum id)))
      ((ref)        (lambda (k vocab addr) (k vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-address env id)           ((env 'address) (lambda (id) #f)
                                                       (syntax-provenance-set id #f)))
(define (env-ref     env vocab addr)   ((env 'ref)     (lambda (vocab addr) #f) vocab addr))
(define (env-ref^    env vocab id)     (let ((addr (env-address env id)))
                                         (and addr (env-ref env vocab addr))))
(define (env-bind!   env id addr)      ((env 'bind!)   id addr))
(define (env-set!    env vocab addr v) ((env 'set!)    vocab addr v))
