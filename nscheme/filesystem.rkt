#lang racket/base
(provide
  local-path
  nscm-read
  nscm-write
  )

(require
  racket/path
  racket/vector
  )

(define (local-path rpath)
  (build-path (path-only (path->complete-path (find-system-path 'run-file)))
              rpath))

(define (nscm-read . args)
  (let convert ((d (apply read args)))
    (cond ((string? d) (string->symbol d))
          ((pair? d) (cons (convert (car d)) (convert (cdr d))))
          ((vector? d) (vector-map convert d))
          (else d))))

(define (nscm-write . args) (apply write args))
