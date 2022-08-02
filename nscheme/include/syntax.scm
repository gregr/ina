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

   (define (make-qualified x vocab=>value) (svector 'qualified x vocab=>value))
   (define (qualified?             x)      (and (svector? x)
                                                (= (svector-length x) 3)
                                                (eq? (svector-ref x 0) 'qualified)))
   (define (qualified-datum        q)      (svector-ref q 1))
   (define (qualified-vocab=>value q)      (svector-ref q 2))

   (define (make-syntax mark* datum provenance) (svector 'syntax mark* datum provenance))

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
           (equal? (car m*) m)
           (make-syntax (cdr m*) (syntax-peek0 s) (syntax-provenance s)))))

  (define (identifier?  s) (and (syntax-wrapped? s) (symbol? (syntax-peek s))))
  (define (identifier?! s) (has-type?! identifier? 'identifier? s))

  (define (identifier-qualifier id)
    (identifier?! id)
    (let ((x (syntax-peek0 id))) (and (qualified? x) (qualified-vocab=>value x))))

  (define (identifier-unqualify id)
    (if (identifier-qualifier id)
        (make-syntax (syntax-mark* id) (syntax-peek id) (syntax-provenance id))
        id))

  (define (identifier-qualify id vocab=>value)
    (if (identifier-qualifier id)
        id
        (make-syntax (syntax-mark* id)
                     (make-qualified (syntax-peek id) vocab=>value)
                     (syntax-provenance id)))))

(define (fresh-identifier name)
  (let ((name (syntax-peek name)))
    (unless (symbol? name) (error "not a symbol" name))
    (syntax-add-mark name (fresh-mark))))

(define (syntax-add-mark s m) (syntax-wrap s (list m)))

(define (free-identifier=?/env env a b)
  (identifier?! a) (identifier?! b)
  (let ((addr.a (env-address env a)) (addr.b (env-address env b)))
    (if (or addr.a addr.b)
        (eq? addr.a addr.b)
        (eq? (syntax-peek a) (syntax-peek b)))))

(define (bound-identifier=? a b)
  (identifier?! a) (identifier?! b)
  (and (eq?    (syntax-peek a) (syntax-peek b))
       (equal? (syntax-mark* a) (syntax-mark* b))))

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
    (let strip ((d0 (if (syntax-wrapped? x) (syntax-peek x) x)))
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

;; hygienic? should be used to ensure that no raw symbols or unmarked identifiers appear anywhere
;; in a hygienic transcription output.
;;
;; Raw symbols are dangerous to produce during hygienic transcription.  If the definition of a
;; syntax transformer that produces a raw symbol was itself produced by a hygienic transcription,
;; the raw symbol produced will not retain the mark from that transcription which produced the
;; definition.  This means a raw symbol will not reliably refer to an identifier bound in the
;; transformer definition's environment.
;;
;; Additionally, all intended identifiers, including those that have not been produced during
;; hygienic transcription, should include at least one mark.  Any unmarked identifier encountered is
;; assumed to have been produced inadvertently from a raw symbol, and should also be ruled out.
(define (hygienic? x)
  (cond ((pair?           x) (and (hygienic? (car x)) (hygienic? (cdr x))))
        ((vector?         x) (hygienic? (vector->list x)))
        ((syntax-wrapped? x) (or (pair? (syntax-mark* x)) (hygienic? (syntax-peek x))))
        (else                (not (symbol? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environments with vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                               (else
                                 (cons (car v=>v) (loop (cdr v=>v) (cons (caar v=>v) v*))))))
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
                                     (rem1p (lambda (entry) (eq? addr (car entry)))
                                            addr=>vocab=>value)))
                             (cons (cons addr (list (cons vocab value)))
                                   addr=>vocab=>value))))))
        (else      (error "invalid environment operation" method))))))

(define env.empty
  (lambda (method)
    (case method
      ((address)    (lambda (k id)         (syntax->datum id)))
      ((ref)        (lambda (k vocab addr) (k vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-address env id)           ((env 'address) (lambda (id) #f) id))
(define (env-ref     env vocab addr)   ((env 'ref)     (lambda (vocab addr) #f) vocab addr))
(define (env-ref^    env vocab id)     (or (let ((addr (env-address env id)))
                                             (and addr (env-ref env vocab addr)))
                                           (qualifier-ref (identifier-qualifier id) vocab)))
(define (env-bind!   env id addr)      ((env 'bind!)   (identifier-unqualify id) addr))
(define (env-set!    env vocab addr v) ((env 'set!)    vocab addr v))
(define (env-qualify env id)           (if (identifier-qualifier id)
                                           id
                                           (let ((addr (env-address env id)))
                                             (identifier-qualify
                                               id (if addr
                                                      ((env 'qualify) (lambda (a v*) '()) '() addr)
                                                      '())))))

(define (qualifier-ref q vocab) (and q (let ((entry (assoc vocab q))) (and entry (cdr entry)))))
