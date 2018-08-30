#lang racket/base
(provide
  local-path
  s->ns
  ns->s
  ns->s/write
  db:lib-path
  )

(require
  racket/path
  racket/vector
  )

(define (local-path rpath)
  (build-path (path-only (path->complete-path (find-system-path 'run-file)))
              rpath))
(define db:lib-path (local-path (build-path "db" "lib.scm")))

(define (s->ns d)
  (cond ((symbol? d) (symbol->string d))
        ((pair? d) (cons (s->ns (car d)) (s->ns (cdr d))))
        ((vector? d) (vector-map s->ns d))
        (else d)))

(define (ns->s d)
  (cond ((string? d) (string->symbol d))
        ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
        ((vector? d) (vector-map ns->s d))
        (else d)))

(define (ns->s/write d)
  (cond ((string? d) (string->symbol d))
        ((pair? d) (cons (ns->s (car d)) (ns->s (cdr d))))
        ((vector? d) (vector-map ns->s d))
        ((or (boolean? d) (null? d) (number? d) (char? d)) d)
        (else (error "cannot write:" d))))
