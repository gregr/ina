#lang racket/base
(provide
  local-path
  )

(require
  racket/path
  )

(define (local-path rpath)
  (build-path (path-only (path->complete-path (find-system-path 'run-file)))
              rpath))
