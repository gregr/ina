#lang racket/base
(provide
  match
  match/==
  )

(require (for-syntax racket/base))

(define-syntax match
  (syntax-rules () ((_ body ...) (match/== equal? body ...))))

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
  (syntax-rules (quote quasiquote cons list list* vector)
    ((m ==? s succeed fail 'datum) (if (==? 'datum s) succeed fail))
    ((m ==? s succeed fail `qq)    (match-pat-qq ==? s succeed fail qq))
    ((m ==? s succeed fail (cons pa pd))
     (if (pair? s) (let ((a (car s)) (d (cdr s)))
                     (match-pat ==? a
                                (match-pat ==? d succeed fail pd)
                                fail pa))
       fail))
    ((m ==? s succeed fail (list* pat)) (match-pat ==? s succeed fail pat))
    ((m ==? s succeed fail (list* p0 pat ...))
     (match-pat ==? s succeed fail (cons p0 (list* pat ...))))
    ((m ==? s succeed fail (list pat ...))
     (match-pat ==? s succeed fail (list* pat ... '())))
    ((m ==? s succeed fail (vector pat ...))
     (if (vector? s) (let ((s-list (vector->list s)))
                       (match-pat ==? s-list succeed fail (list pat ...)))
       fail))
    ((m ==? s succeed fail literal) (match-pat ==? s succeed fail 'literal))))

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
     (match-pat ==? s succeed fail (cons `qqa `qqd)))
    ((_ ==? s succeed fail datum) (match-pat ==? s succeed fail 'datum))))
