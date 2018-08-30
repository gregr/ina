#lang racket/base
(provide
  (all-from-out "nscheme.rkt")
  env
  env-extend
  db:lib
  import/env
  s->ns
  ns->s
  )

(require
  "filesystem.rkt"
  "nscheme.rkt"
  "nscheme-module.rkt"
  )

(define db:lib/s (ns->s (with-input-from-file db:lib-path read)))
(define db:lib (s->ns db:lib/s))

(define env '())
(define (env-extend library-name)
  (define name (string->symbol library-name))
  (set! env (link/module* env (map cdr (library-get db:lib/s name)))))

(define-syntax import/env
  (syntax-rules ()
    ((_ name ...)
     (begin (define name (cdr (or (assoc 'name env)
                                  (error "unknown import:" 'name)))) ...))))

(env-extend 'data)
(env-extend 'nscheme)
