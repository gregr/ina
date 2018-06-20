#lang racket/base
(provide
  environment?
  env-empty
  env-extend
  env-ref-lexical env-ref-transformer env-ref-parser
  env-hide! env-alias!
  env-bind*! env-bind-transformer*! env-bind-parser*!

  labeled-name labeled-name?
  syntax-close closed-name? closed-name-env closed-name-n
  syntax-open syntax-open?
  name->symbol

  syntax=? match-syntax
  )

(require
  "match.rkt"
  "../type.rkt"
  racket/vector
  )

;;; Syntactic environments
(define-type*
  address?
  (addr-transformer addr-transformer? addr-transformer-proc)
  (addr-parser addr-parser? addr-parser-proc)
  (addr-lexical addr-lexical? addr-lexical-v)
  (addr-unbound addr-unbound? addr-unbound-v))
(define-type environment environment? env-frames)
(define env-empty (environment '()))
;; Environments use mutable frames to support late-binding definition contexts.
(define (env-extend env) (environment (cons (box (hash)) (env-frames env))))
(define (env-set*! env b*)
  (define frame (car (env-frames env)))
  (set-box! frame (foldl (lambda (b h)
                           (if (car b) (hash-set h (car b) (cdr b)) h))
                         (unbox frame) b*)))
(define (env-ref/default env n default)
  (let loop ((frames (env-frames env)))
    (cond ((null? frames) default)
          (else (define value (hash-ref (unbox (car frames)) n #f))
                (if value value (loop (cdr frames)))))))
(define (env-ref env n) (env-ref/default env n n))
(define (env-ref-lexical env n)
  (define addr (env-ref env n))
  (when (not (addr-lexical? addr)) (error "unbound variable:" n))
  (addr-lexical-v addr))
(define (env-ref-transformer env n)
  (define addr (env-ref env n))
  (and (addr-transformer? addr) (addr-transformer-proc addr)))
(define (env-ref-parser env n)
  (define addr (env-ref env n))
  (and (addr-parser? addr) (addr-parser-proc addr)))
(define (env-hide! env n)
  (env-set*! env `((,n . ,(addr-unbound (fresh-name (name->symbol n)))))))
(define (env-alias! env n aliased)
  (env-set*! env `((,n . ,(env-ref env aliased)))))
(define (env-bind*! env b*)
  (env-set*! env (map (lambda (b) (cons (car b) (addr-lexical (cdr b)))) b*)))
(define (env-bind-transformer*! env b*)
  (env-set*! env (map (lambda (b) (cons (car b) (addr-transformer (cdr b))))
                      b*)))
(define (env-bind-parser*! env b*)
  (env-set*! env (map (lambda (b) (cons (car b) (addr-parser (cdr b))))
                      b*)))

;;; Names
;; Closed names are late-resolved to support recursive definition contexts.
(define-vector-type
  (closed closed-name) closed-name? closed-name-env closed-name-n)
(define-vector-type
  (name labeled-name) labeled-name? labeled-name-sym labeled-name-l)
(define fresh-name (let ((label 0)) (lambda (n) (set! label (+ 1 label))
                                      (labeled-name n label))))
(define (name? n) (or (symbol? n) (labeled-name? n)))
(define (name->symbol n) (cond ((symbol? n) n)
                               ((labeled-name? n) (labeled-name-sym n))
                               (else (error "invalid name:" n))))

;;; Syntax
(define-type syntax-open syntax-open? syntax-open-form)
(define (syntax-close env stx)
  (let loop ((stx stx))
    (cond ((syntax-open? stx) (syntax-open-form stx))
          ((name? stx)        (closed-name env stx))
          ((pair? stx)        (cons (loop (car stx)) (loop (cdr stx))))
          ((vector? stx)      (vector-map loop stx))
          (else               stx))))
(define (syntax-resolve env n)
  (cond ((name? n)        (env-ref env n))
        ((closed-name? n) (syntax-resolve (closed-name-env n)
                                          (closed-name-n n)))
        (else             n)))
(define (syntax=? env a b)
  (define (=? x y) (syntax=? env x y))
  (cond ((and (pair? a) (pair? b))
         (and (=? (car a) (car b)) (=? (cdr a) (cdr b))))
        ((and (vector? a) (vector? b)) (=? (vector->list a) (vector->list b)))
        (else (equal? (syntax-resolve env a) (syntax-resolve env b)))))
(define-syntax match-syntax
  (syntax-rules ()
    ((_ env body ...) (let* ((e env) (=? (lambda (a b) (syntax=? e a b))))
                        (match/== =? body ...)))))
