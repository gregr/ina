#lang racket/base
(provide
  (all-from-out "nscheme.rkt")
  env-current
  env-extend
  libraries
  import/env

  eval-ast
  )

(require
  "filesystem.rkt"
  "nscheme.rkt"
  "nscheme-module.rkt"
  )

(define libraries (with-input-from-file (local-path "lib.db.scm") nscm-read))

(define env '())
(define (env-current) env)
(define (env-extend library-name)
  (define rib (assoc (string->symbol library-name) libraries))
  (define library (cdr (or rib (error "unknown library:" library-name))))
  (set! env (foldl link/module env (map eval/module (map cdr library)))))

(define-syntax import/env
  (syntax-rules ()
    ((_ name ...)
     (begin (define name (cdr (or (assoc 'name env)
                                  (error "unknown import:" 'name)))) ...))))

(env-extend 'data)
(env-extend 'nscheme)
(import/env eval-ast)
