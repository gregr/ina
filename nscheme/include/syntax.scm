;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax with lazy mark propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: eliminate description, which we shouldn't need thanks to AST provenance.
(define (fresh-address description) (mvector description))

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

(define env.empty
  (lambda (method)
    (caseq method
      ((ref)  (lambda (fail id) (fail)))
      ((set!) (error "invalid immutable environment operation" method))
      (else   (error "invalid environment operation"           method)))))

(define (env-extend env env.first)
  (lambda (method)
    (caseq method
      ((ref)  (lambda (fail id) ((env.first 'ref) (lambda () ((env 'ref) fail id)) id)))
      ((set!) (error "invalid immutable environment operation" method))
      (else   (error "invalid environment operation"           method)))))

(define (env-add-mark env m)
  (lambda (method)
    (caseq method
      ((ref)  (lambda (fail id) (let ((i (and (identifier? id) (syntax-remove-mark? id m))))
                                  (if i ((env 'ref) fail i) (fail id)))))
      ((set!) (error "invalid immutable environment operation" method))
      (else   (error "invalid environment operation"           method)))))

(splicing-local
  ((define id=? bound-identifier=?)
   (define id-dict.empty '())
   (define (id-dict-ref id=>x id)
     (let ((kv (assp (lambda (id.key) (id=? id.key id)) id=>x)))
       (and kv (cdr kv))))
   (define (id-dict-set id=>x id x)
     (let ((id=>x (let loop ((ix* id=>x) (skipped '()))
                    (cond ((null? ix*)          id=>x)
                          ((id=? (caar ix*) id) (foldl cons (cdr ix*) skipped))
                          (else                 (loop (cdr ix*) (cons (car ix*) skipped)))))))
       (cons (cons id x) id=>x))))
  (define (make-env)
    (let ((id=>x id-dict.empty))
      (lambda (method)
        (caseq method
          ((ref)  (lambda (fail id) (or (id-dict-ref id=>x id) (fail))))
          ((set!) (lambda (id x)    (set! id=>x (id-dict-set id=>x id x))))
          (else   (error "invalid environment operation" method)))))))

(define (env-set! env id x) ((env 'set!) id x))
(define (env-ref  env id)   ((env 'ref) (lambda () #f) id))
