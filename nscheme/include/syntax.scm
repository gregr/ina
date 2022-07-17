;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark-based renaming ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (mvector))
(define antimark     #f)

(splicing-let
  ((marks-append
     (let ((antimark? not))
       (lambda (m*.outer m*.inner)
         (if (null? m*.inner)
           m*.outer
           (let loop ((m* (reverse m*.outer)) (m*.final m*.inner))
             (cond ((null? m*)                 m*.final)
                   ((antimark? (car m*.final)) (let ((m*.final (cdr m*.final)))
                                                 (if (null? m*.final)
                                                   (reverse m*)
                                                   (loop (cdr m*) m*.final))))
                   (else                       (foldl cons m*.final m*))))))))
   (make-syntax  (lambda (marks datum provenance) (svector 'syntax marks datum provenance)))
   (syntax-marks (lambda (s)                      (svector-ref s 1)))
   (syntax-datum (lambda (s)                      (svector-ref s 2))))

  (define (syntax-provenance s) (syntax?! s) (svector-ref s 3))
  (define (syntax? x)           (and (svector? x)
                                     (= (svector-length x) 4)
                                     (eq? (svector-ref x 0) 'syntax)))
  (define (syntax?! x)          (has-type?! syntax? 'syntax? x))

  (define (identifier?   s) (and (syntax? s) (symbol? (syntax-datum s))))
  (define (identifier?!  s) (has-type?! identifier? 'identifier? s))
  (define (identifier-id s)
    (identifier?! s)
    (let ((m* (syntax-marks s)))
      (if (null? m*)
        (syntax-datum s)
        (cons (syntax-datum s) m*))))

  (define (bound-identifier=? a b)
    (equal? (identifier-id a) (identifier-id b)))

  (define (free-identifier=?  env a b)
    (identifier?! a) (identifier?! b)
    (equal? (env-address env a) (env-address env b)))

  (define (datum->syntax/provenance context datum provenance)
    (let ((m* (cond ((not context) '())
                    (else          (syntax?! context) (syntax-marks context)))))
      (if (syntax? datum)
        (syntax-mark* datum m*)
        (make-syntax m* datum provenance))))

  (define (datum->syntax context datum)
    (datum->syntax/provenance context datum #f))

  (define (syntax->datum s)
    (define (strip d0)
      (cond ((pair?   d0) (cons (loop (car d0)) (loop (cdr d0))))
            ((vector? d0) (vector-map loop d0))
            (else         d0)))
    (define (loop x) (strip (if (syntax? x) (syntax-datum x) x)))
    (syntax?! s)
    (strip (syntax-datum s)))

  (define (syntax-mark* s m*)
    (syntax?! s)
    (make-syntax (marks-append m* (syntax-marks s))
                 (syntax-datum s)
                 (syntax-provenance s)))

  (define (syntax-mark s m) (syntax-mark* s (list m)))

  (define (syntax-unmark s m)
    (syntax?! s)
    (let ((m* (syntax-marks s)))
      (and (pair? m*)
           (equal? (car m*) m)
           (make-syntax (cdr m*) (syntax-datum s) (syntax-provenance s)))))

  (define (syntax-unwrap s)
    (syntax?! s)
    (let ((d (syntax-datum s)) (m* (syntax-marks s)))
      (define (propagate x) (if (syntax? x) (syntax-mark* x m*) (make-syntax m* x #f)))
      (cond ((pair?   d) (cons (propagate (car d)) (propagate (cdr d))))
            ((vector? d) (vector-map propagate d))
            (else        d))))

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
               (loop (cdr x) (cons (car x) parts))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments with vocabularies ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-address description) (mvector description))

(define (env:inherit env.next env.first)
  (lambda (method)
    (case method
      ((address)    (lambda (id)         (or (env-address env.first id)
                                             (env-address env.next  id))))
      ((ref)        (lambda (vocab addr) (or (env-ref env.first vocab addr)
                                             (env-ref env.next  vocab addr))))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env:unmark env m)
  (lambda (method)
    (case method
      ((address)    (lambda (id)         (and (identifier? id)
                                              (let ((id (syntax-unmark id m)))
                                                (and id (env-address env id))))))
      ((ref)        (lambda (vocab addr) (env-ref env vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env:scope)
  (let ((id=>addr (make-hash)) (addr=>vocab=>value (make-hash)))
    (lambda (method)
      (case method
        ((address) (lambda (id)      (hash-ref  id=>addr id #f)))
        ((bind!)   (lambda (id addr) (hash-set! id=>addr id addr)))
        ((ref)     (lambda (vocab addr)
                     (and addr (hash-ref (hash-ref addr=>vocab=>value addr (hash)) vocab #f))))
        ((set!)    (lambda (vocab addr value)
                     (and addr (hash-update!
                                 addr=>vocab=>value addr
                                 (lambda (vocab=>value) (hash-set vocab=>value vocab value))
                                 (hash)))))
        (else      (error "invalid environment operation" method))))))

(define (env-address env       id)        ((env 'address)    id))
(define (env-bind!   env       id   addr) ((env 'bind!)      id addr))
(define (env-ref     env vocab addr)      ((env 'ref)  vocab addr))
(define (env-set!    env vocab addr v)    ((env 'set!) vocab addr v))
(define (env-ref^    env vocab id)        (env-ref env vocab (env-address env id)))
(define (env-ref*^   env vocab ids)       (map (lambda (id)   (env-ref^ env vocab id))   ids))
(define (env-ref*    env vocab addrs)     (map (lambda (addr) (env-ref  env vocab addr)) addrs))
(define (env-set!**  env vocab addrs vs)  (for-each (lambda (addr v) (env-set! env vocab addr v)) addrs vs))
(define (env-set!*   env vocab . addrvs)  (let ((addrvs (plist->alist addrvs)))
                                            (env-set!** env vocab (map car addrvs) (map cdr addrvs))))
(define (env-bind!** env       ids addrs) (for-each (lambda (id addr) (env-bind! env id addr)) ids addrs))
(define (env-bind!*  env       . idaddrs) (let ((idaddrs (plist->alist idaddrs)))
                                            (env-bind!** env (map car idaddrs) (map cdr idaddrs))))
