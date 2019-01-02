#lang racket/base
(provide (rename-out (nscm:printf printf)
                     (nscm:file-exists? file-exists?)
                     (nscm:read*/file   read*/file)
                     (nscm:write/file   write/file)))
(require "interop.rkt")

;; TODO: eventually replace these with lower level file capabilities.
(define (nscm:printf . args)  (apply printf args) #t)
(define (nscm:file-exists? p) (file-exists? (path:ns->s p)))
(define (nscm:read*/file p)   (s->ns (read*/file (path:ns->s p))))
(define (nscm:write/file p d) (write/file (path:ns->s p) (racket-datum d)) #t)
