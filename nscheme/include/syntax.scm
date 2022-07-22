;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark-based renaming ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-mark) (mvector))
(define antimark     #f)

(splicing-let
  ((marks-append
     (let ((antimark? not))
       (lambda (m*.outer m*.inner)
         (cond ((null? m*.inner)                 m*.outer)
               ((null? m*.outer)                 m*.inner)
               ((not (antimark? (car m*.inner))) (append m*.outer m*.inner))
               (else (let loop ((m (car m*.outer)) (m* (cdr m*.outer)))
                       (cond ((null? m*) (cdr m*.inner))
                             (else       (cons m (loop (car m*) (cdr m*)))))))))))
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

(define (env-extend env env.first)
  (lambda (method)
    (case method
      ((address)    (lambda (k ident)      ((env.first 'address)
                                            (lambda (ident) ((env 'address) k ident))
                                            ident)))
      ((ref)        (lambda (k vocab addr) ((env.first 'ref)
                                            (lambda (vocab addr) ((env 'ref) k vocab addr))
                                            vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-mark env m)
  (lambda (method)
    (case method
      ((address)    (lambda (k ident)      (let ((i (and (identifier? ident)
                                                         (syntax-unmark ident m))))
                                             (if i ((env 'address) k i) (k ident)))))
      ((ref)        (lambda (k vocab addr) (env-ref env vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (make-env)
  (let ((id=>addr (make-hash)) (addr=>vocab=>value (make-hash)))
    (lambda (method)
      (case method
        ((address) (lambda (k ident)      (hash-ref id=>addr ident (lambda () (k ident)))))
        ((ref)     (lambda (k vocab addr) (hash-ref (hash-ref addr=>vocab=>value addr (hash))
                                                    vocab (lambda () (k vocab addr)))))
        ((bind!)   (lambda (ident addr)   (hash-set! id=>addr ident addr)))
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
      ((address)    (lambda (k ident)      (syntax->datum ident)))
      ((ref)        (lambda (k vocab addr) (k vocab addr)))
      ((bind! set!) (error "invalid immutable environment operation" method))
      (else         (error "invalid environment operation"           method)))))

(define (env-address env ident)        ((env 'address) (lambda (ident) #f) ident))
(define (env-ref     env vocab addr)   ((env 'ref)     (lambda (vocab addr) #f) vocab addr))
(define (env-ref^    env vocab ident)  (let ((addr (env-address env ident)))
                                         (and addr (env-ref env vocab addr))))
(define (env-bind!   env ident addr)   ((env 'bind!)   ident addr))
(define (env-set!    env vocab addr v) ((env 'set!)    vocab addr v))
