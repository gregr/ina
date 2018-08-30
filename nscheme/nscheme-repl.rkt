#lang racket/base
(provide
  (all-from-out "nscheme.rkt")
  env-current
  env-extend
  lib
  import/env
  )

(require
  "filesystem.rkt"
  "nscheme.rkt"
  "nscheme-module.rkt"
  )

(define lib (ns->s (with-input-from-file db:lib-path read)))

(define env '())
(define (env-current) env)
(define (env-extend library-name)
  (define name (string->symbol library-name))
  (set! env (link/module* env (map cdr (library-get lib name)))))

(define-syntax import/env
  (syntax-rules ()
    ((_ name ...)
     (begin (define name (cdr (or (assoc 'name env)
                                  (error "unknown import:" 'name)))) ...))))

(env-extend 'data)
(env-extend 'nscheme)
