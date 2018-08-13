#lang racket/base
(provide
  make-mvector
  mvector?
  mvector-length
  mvector-ref
  mvector-set!
  mvector->vector
  case
  match
  match/==  ;; TODO: generalize to match/syntax.
  define-vector-type
  define-vector-type*
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
  racket/vector
  (for-syntax racket/base))

;; TODO:
;; Support for #f parameters in define, lambda, let, etc.

(struct mvector (v) #:transparent)
(define (make-mvector k d) (mvector (make-vector k d)))
(define (mvector-length mv) (vector-length (mvector-v mv)))
(define (mvector-ref mv i) (vector-ref (mvector-v mv) i))
(define (mvector-set! mv i d) (vector-set! (mvector-v mv) i d))
(define (mvector->vector mv) (vector-copy (mvector-v mv)))

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

(define-syntax match
  (syntax-rules () ((_ body ...) (match/== new-equal? body ...))))

(define-syntax match/==
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

(define-syntax define-vector-type*
  (syntax-rules ()
    ((_ family? (vt vt? vtf ...) ...)
     (begin (define (family? d) (or (vt? d) ...))
            (define-vector-type vt vt? vtf ...) ...))))

(define-syntax define-vector-type
  (syntax-rules ()
    ((_ (name tag) name?)
     (define-vector-type/singleton tag name name?))
    ((_ (construct tag) name? f* ...)
     (define-vector-type/multi tag construct name? f* ...))
    ((_ name name? f* ...)
     (define-vector-type (name (new-quote name)) name? f* ...))))

(define-syntax define-vector-type/singleton
  (syntax-rules ()
    ((_ tag name name?)
     (begin (define name (vector tag))
            (define (name? datum) (new-equal? name datum))))))

(define-syntax define-vector-type/multi
  (syntax-rules ()
    ((_ tag construct name? f* ...)
     (begin (define t tag)
            (define (name? datum) (and (vector? datum)
                                       (<= 1 (vector-length datum))
                                       (new-equal? t (vector-ref datum 0))))
            (define-vector-type/multi-etc name? t construct 1 (f* ...) ())))))

(define-syntax define-vector-type/multi-etc
  (syntax-rules ()
    ((_ name? tag construct index ((get set) f* ...) field*)
     (begin (define-vector-type/multi-etc
              name? tag construct index (get f* ...) field*)
            (define (set datum value)
              (if (name? datum)
                (let ((new (vector-copy datum)))
                  (vector-set! new index value) new)
                (error (format "~a with wrong argument type:" 'set) datum)))))
    ((_ name? tag construct index (get f* ...) (field* ...))
     (begin (define-vector-type/multi-etc
              name? tag construct (+ 1 index) (f* ...) (field* ... get))
            (define (get datum)
              (if (name? datum)
                (vector-ref datum index)
                (error (format "~a with wrong argument type:" 'get) datum)))))
    ((_ name? tag construct _ () (field* ...))
     (define (construct field* ...) (vector tag field* ...)))))
