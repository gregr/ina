#lang racket/base
(provide
  nsmod
  nsmod?
  nsmod-required
  nsmod-provided
  nsmod-code
  nsmod-proc
  lambda/module
  apply/module
  link/module
  rename
  )

(require
  "nscheme.rkt"
  )

(define-vector-type
  nsmod nsmod? nsmod-required nsmod-provided nsmod-code nsmod-proc)

(define rename (void))

(define-syntax lambda/module
  (syntax-rules ()
    ((_ body ...) (lambda/module-etc () () body ...))))
(define-syntax lambda/module-etc
  (syntax-rules (provide require rename)
    ((_ is es (provide) b ...) (lambda/module-etc is es b ...))
    ((_ is (es ...) (provide (rename (old new) ...) items ...) b ...)
     (lambda/module-etc is (es ... (old new) ...) (provide items ...) b ...))
    ((_ is (es ...) (provide name items ...) b ...)
     (lambda/module-etc is (es ... (name name)) (provide items ...) b ...))

    ((_ is es (require) b ...) (lambda/module-etc is es b ...))
    ((_ (is ...) es (require (rename (old new) ...) items ...) b ...)
     (lambda/module-etc (is ... (old new) ...) es (require items ...) b ...))
    ((_ (is ...) es (require name items ...) b ...)
     (lambda/module-etc (is ... (name name)) es (require items ...) b ...))

    ((_ ((iold inew) ...) ((eold enew) ...) body ...)
     (nsmod '(iold ...) '(enew ...) '(body ...)
            (lambda (inew ...) body ... (list (cons 'enew eold) ...))))))

(define (apply/module m env)
  (define rs (map (lambda (r) (cdr (assoc r env))) (nsmod-required m)))
  (apply (nsmod-proc m) rs))

(define (link/module env ms)
  (foldl (lambda (m env) (append (apply/module m env) env)) env ms))
