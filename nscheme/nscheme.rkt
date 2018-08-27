#lang racket/base
(provide
  reverse-append
  boolean=?
  filter-not
  range
  string->vector
  vector->string
  vector-set
  make-mvector
  mvector?
  mvector=?
  mvector-length
  mvector-ref
  mvector-set!
  mvector->vector
  case
  and/let*
  export
  import
  import/apply
  import->lambda
  (rename-out
    (new-symbol? symbol?)
    (new-symbol=? symbol=?)
    (new-equal? equal?)
    (new-read read)
    (new-write write)
    (new-quote quote)
    (new-quasiquote quasiquote)
    ))

(require
  racket/bool
  racket/list
  racket/vector
  (for-syntax racket/base))

;; TODO:
;; Support for #f parameters in define, lambda, let, etc.

(define (reverse-append xs ys)
  (if (null? xs) ys (reverse-append (cdr xs) (cons (car xs) ys))))

(struct mvector (v) #:transparent)
(define (mvector=? m n) (eq? m n))
(define (make-mvector k d) (mvector (make-vector k d)))
(define (mvector-length mv) (vector-length (mvector-v mv)))
(define (mvector-ref mv i) (vector-ref (mvector-v mv) i))
(define (mvector-set! mv i d) (vector-set! (mvector-v mv) i d))
(define (mvector->vector mv) (vector-copy (mvector-v mv)))

(define (vector-set v i d)
  (define new (vector-copy v))
  (vector-set! new i d)
  new)

(define (string->vector s) (list->vector (string->list s)))
(define (vector->string v) (list->string (vector->list v)))

(define (new-symbol? d) (string? d))
(define (new-symbol=? a b) (string=? a b))

(define (new-equal? a b)
  (or (eqv? a b)
      (and (new-symbol? a) (new-symbol? b) (new-symbol=? a b))
      (and (pair? a) (pair? b)
           (new-equal? (car a) (car b))
           (new-equal? (cdr a) (cdr b)))
      (and (vector? a) (vector? b)
           (new-equal? (vector->list a) (vector->list b)))))

(define (new-read . args)
  (let convert ((d (apply read args)))
    (cond ((symbol? d) (symbol->string d))
          ((pair? d) (cons (convert (car d)) (convert (cdr d))))
          ((vector? d) (vector-map convert d))
          (else d))))

(define (new-write d . args)
  (define (convert d)
    (cond ((new-symbol? d) (string->symbol d))
          ((pair? d) (cons (convert (car d)) (convert (cdr d))))
          ((vector? d) (vector-map convert d))
          ((mvector? d) (error "cannot write mvector:" d))
          ((procedure? d) (error "cannot write procedure:" d))
          (else d)))
  (apply write (convert d) args))

(define-syntax (new-quote stx)
  (syntax-case stx ()
    ((_ id)       (identifier? #'id)
                  #`(new-quote #,(symbol->string (syntax->datum #'id))))
    ((_ (a . d))  #'(cons (new-quote a) (new-quote d)))
    ((_ #(d ...)) #'(vector (new-quote d) ...))
    ((_ d)        #'(quote d))))

(define-syntax new-quasiquote (syntax-rules () ((_ d) (new-qq () d))))
(define-syntax new-qq
  (syntax-rules (new-quasiquote unquote unquote-splicing)
    ((_ lvl (new-quasiquote d))
     (list (new-quote quasiquote)       (new-qq (s . lvl) d)))
    ((_ (s . p) (unquote e))
     (list (new-quote unquote)          (new-qq p e)))
    ((_ (s . p) (unquote-splicing e))
     (list (new-quote unquote-splicing) (new-qq p (unquote-splicing e))))

    ((_ () (unquote e))                e)
    ((_ () ((unquote-splicing e) . d)) (append e (new-qq () d)))
    ((_ lvl unquote)                   (error "invalid unquote"))
    ((_ lvl unquote-splicing)          (error "invalid unquote-splicing"))
    ((_ lvl (a . d))                   (cons (new-qq lvl a) (new-qq lvl d)))
    ((_ lvl #(d ...))                  (list->vector (new-qq lvl (d ...))))
    ((_ lvl d)                         (new-quote d))))

(define-syntax case
  (syntax-rules () ((_ e c ...) (let ((x e)) (case-etc x c ...)))))
(define-syntax case-etc
  (syntax-rules (else)
    ((_ x)                          (error "no matching case:" x))
    ((_ x (else body ...))          (let () body ...))
    ((_ x ((d ...) body ...) c ...) (if (or (new-equal? x (new-quote d)) ...)
                                      (let () body ...)
                                      (case-etc x c ...)))))

(define-syntax and/let*
  (syntax-rules ()
    ((_ () body ...) (and body ...))
    ((_ ((lhs rhs) bindings ...) body ...)
     (let ((lhs rhs)) (and lhs (and/let* (bindings ...) body ...))))))

(define-syntax import
  (syntax-rules ()
    ((_ (name ...) body ...)
     (cons (new-quote (name ...)) (lambda (name ...) body ...)))))

(define-syntax export
  (syntax-rules ()
    ((_ name ...) (list (cons (new-quote name) name) ...))))

(define (import->lambda i) (lambda (env) (import/apply i env)))

(define (import/apply i env)
  (apply (cdr i) (map (lambda (name)
                        (cdr (or (assoc name env)
                                 (error "missing argument:" name)))) (car i))))
