#lang racket/base
(provide case mlet mdefine aquote syntax-case-simple syntax-rules-simple)
(require "primitive.rkt" (prefix-in rkt: racket/base) (prefix-in rkt: racket/pretty))

(read-decimal-as-inexact #f)
(rkt:pretty-print-exact-as-decimal #t)

(define (mistake* detail*) (panic 'mistake detail*))
(define (mistake . detail*) (mistake* detail*))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (case e . clause) (let ((x e)) (case-etc x . clause)))
(define-syntax case-etc
  (syntax-rules (else =>)
    ((_ x)                            (mistake "no matching case" x))
    ((_ x (else => proc))             (proc x))
    ((_ x (else rhs ...))             (let () rhs ...))
    ((_ x ((d ...) rhs ...) . clause) (if (rkt:member x '(d ...))
                                          (let () rhs ...)
                                          (case-etc x . clause)))))

(define-syntax-rule (mlet . body) (let . body))
(define-syntax-rule (mdefine . body) (define . body))

(define-syntax-rule (aquote expr ...) (list (cons 'expr expr) ...))

(define-syntax-rule (syntax-case-simple stx . clause*) (syntax-case stx () . clause*))
(define-syntax-rule (syntax-rules-simple    . clause*) (syntax-rules    () . clause*))
