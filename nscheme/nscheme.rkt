#lang racket/base
(provide
  shift
  reset
  reverse-append
  remove-duplicates
  assoc
  boolean=?
  number=?
  filter-not
  range
  take
  drop
  remf
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
  procedure=?
  case
  match
  match/=?  ;; TODO: generalize to match/syntax.
  and/let*
  export
  import
  import-apply
  import->lambda
  (rename-out
    (new-equal? equal?)
    (new-read read)
    (new-write write)
    (new-quote quote)
    (new-quasiquote quasiquote)
    ))

(require
  "filesystem.rkt"
  racket/bool
  racket/control
  racket/list
  racket/vector
  (for-syntax racket/base))

;; TODO:
;; Support for #f parameters in define, lambda, let, etc.

(define (reverse-append xs ys)
  (if (null? xs) ys (reverse-append (cdr xs) (cons (car xs) ys))))

(define (procedure=? m n) (eq? m n))

(struct mvector (v) #:transparent)
(define (mvector=? m n) (eq? m n))
(define (make-mvector k d) (mvector (make-vector k d)))
(define (mvector-length mv) (vector-length (mvector-v mv)))
(define (mvector-ref mv i) (vector-ref (mvector-v mv) i))
(define (mvector-set! mv i d) (vector-set! (mvector-v mv) i d) #t)
(define (mvector->vector mv) (vector-copy (mvector-v mv)))

(define (vector-set v i d)
  (define new (vector-copy v))
  (vector-set! new i d)
  new)

(define (string->vector s) (list->vector (map char->integer (string->list s))))
(define (vector->string v) (list->string (map integer->char (vector->list v))))

(define (number=? a b) (eqv? a b))

(define (new-equal? a b)
  (or (eqv? a b)
      (and (string? a) (string? b) (string=? a b))
      (and (pair? a) (pair? b)
           (new-equal? (car a) (car b))
           (new-equal? (cdr a) (cdr b)))
      (and (vector? a) (vector? b)
           (new-equal? (vector->list a) (vector->list b)))))

(define (new-read . args) (s->ns (apply read args)))

(define (new-write d . args) (apply write (ns->s/write d) args))

(define (assoc k xs) (cond ((null? xs) #f)
                           ((new-equal? k (caar xs)) (car xs))
                           (else (assoc k (cdr xs)))))

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

(define-syntax match
  (syntax-rules () ((_ body ...) (match/=? new-equal? body ...))))

(define-syntax match/=?
  (syntax-rules ()
    ((_ ==? scrutinee body ...) (match-let-etc ==? scrutinee body ...))))

(define-syntax (match-let-etc stx)
  (syntax-case stx ()
    ((_ ==? scrutinee name body ...)
     (identifier? #'name)
     #'(let name ((x scrutinee)) (match-etc ==? x body ...)))
    ((_ ==? scrutinee body ...)
     #'(let ((x scrutinee)) (match-etc ==? x body ...)))))

(define-syntax match-etc
  (syntax-rules (guard)
    ((_ ==? scrutinee) (error "no matching clause for:" scrutinee))
    ((_ ==? scrutinee (pat (guard condition ...) body ...) clause ...)
     (let ((k-fail (lambda () (match-etc ==? scrutinee clause ...))))
       (match-pat ==? scrutinee
                  (if (and condition ...) (let () body ...) (k-fail))
                  (k-fail) pat)))
    ((_ ==? scrutinee (pat body ...) clause ...)
     (match-etc ==? scrutinee (pat (guard) body ...) clause ...))))

(define-syntax (match-pat stx)
  (syntax-case stx (_)
    ((m ==? s succeed fail _)   #'succeed)
    ((m ==? s succeed fail id)
     (identifier? #'id)         #'(let ((id s)) succeed))
    ((m ==? s succeed fail pat) #'(match-pat-etc ==? s succeed fail pat))))

(define-syntax match-pat-etc
  (syntax-rules (new-quote new-quasiquote cons list list* vector)
    ((m ==? s succeed fail (new-quote datum))
     (if (==? (new-quote datum) s) succeed fail))
    ((m ==? s succeed fail (new-quasiquote qq))
     (match-pat-qq ==? s succeed fail qq))
    ((m ==? s succeed fail (cons pa pd))
     (if (pair? s) (let ((a (car s)) (d (cdr s)))
                     (match-pat ==? a (match-pat ==? d succeed fail pd)
                                fail pa))
       fail))
    ((m ==? s succeed fail (list* pat)) (match-pat ==? s succeed fail pat))
    ((m ==? s succeed fail (list* p0 pat ...))
     (match-pat ==? s succeed fail (cons p0 (list* pat ...))))
    ((m ==? s succeed fail (list pat ...))
     (match-pat ==? s succeed fail (list* pat ... (new-quote ()))))
    ((m ==? s succeed fail (vector pat ...))
     (match-pat-qq ==? s succeed fail #(,pat ...)))
    ((m ==? s succeed fail literal)
     (match-pat ==? s succeed fail (new-quote literal)))))

(define-syntax match-pat-qq
  (syntax-rules (unquote unquote-splicing)
    ((_ ==? s succeed fail ,pat) (match-pat ==? s succeed fail pat))
    ((_ ==? s succeed fail (,@id)) (let ((id s)) (if (list? id) succeed fail)))
    ((_ ==? s succeed fail (,@id . _))
     (error "unquote-splicing pattern must be last"))
    ((_ ==? s succeed fail #(vqq ...))
     (if (vector? s) (let ((s-list (vector->list s)))
                       (match-pat-qq ==? s-list succeed fail (vqq ...)))
       fail))
    ((_ ==? s succeed fail (qqa . qqd))
     (match-pat ==? s succeed fail (cons (new-quasiquote qqa)
                                         (new-quasiquote qqd))))
    ((_ ==? s succeed fail datum)
     (match-pat ==? s succeed fail (new-quote datum)))))

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

(define (import->lambda i) (lambda (env) (import-apply i env)))

(define (import-apply i env)
  (apply (cdr i) (map (lambda (name)
                        (cdr (or (assoc name env)
                                 (error "missing argument:" name)))) (car i))))
