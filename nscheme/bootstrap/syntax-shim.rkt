#lang racket/base
(provide
  apply/values set! case-values case let-values let*-values mlet mdefine aquote)
(require (prefix-in rkt: racket/base) (prefix-in rkt: racket/pretty))

(read-decimal-as-inexact #f)
(rkt:pretty-print-exact-as-decimal #t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (apply/values rator vrand) (call-with-values (lambda () vrand) rator))
(define-syntax-rule (set! x e) (begin (rkt:set! x e) (values)))
(define-syntax-rule (case-values e.values case-clauses ...)
  (apply/values (case-lambda case-clauses ...) e.values))

(define-syntax case
  (syntax-rules (else =>)
    ((_ x)                              (values))
    ((_ x (else rhs ...))               (let () rhs ...))
    ((_ x (=> proc))                    (proc x))
    ((_ x ((d ...) rhs ...) clause ...) (if (rkt:memv x '(d ...))
                                            (let () rhs ...)
                                            (case x clause ...)))))

;; WARNING: these are only complete enough to run our bootstrapping process
(define-syntax let-values
  (syntax-rules ()
    ((_ (((param ...) rhs) ...) body ...)
     (rkt:let-values (((param ...) rhs) ...) body ...))
    ((_ ((param rhs)) body ...)
     (apply/values (lambda param body ...) rhs))))
(define-syntax let*-values
  (syntax-rules ()
    ((_ (((param ...) rhs) bpair* ...) body ...)
     (let-values (((param ...) rhs)) (let*-values (bpair* ...) body ...)))
    ((_ ((param rhs) bpair* ...) body ...)
     (apply/values (lambda param (let*-values (bpair* ...) body ...)) rhs))
    ((_ () body ...) (let () body ...))))

(define-syntax-rule (mlet . body) (let . body))
(define-syntax-rule (mdefine . body) (define . body))

(define-syntax-rule (aquote expr ...) (list (cons 'expr expr) ...))
