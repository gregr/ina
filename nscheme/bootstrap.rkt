#lang racket/base
(require
  "interop.rkt"
  "stage.rkt"
  racket/match
  racket/runtime-path
  )

;; TODO: incorporate all of stage.rkt here.

(module+ main
  (define-runtime-path here ".")
  (define (read/file:nscm path) (s->ns (read/file (build-path here path))))
  (define capabilities (s->ns `((printf    . ,(lift printf))
                                (read/file . ,(lift read/file:nscm))
                                (eval      . ,(lift base:eval)))))
  (define bootstrap.scm
    (s->ns `(let () ,@(read/file (build-path here "bootstrap.scm")))))
  (define nscheme.scm.rkt
    (time ($apply (base:eval bootstrap.scm) (list capabilities))))
  ;; TODO:
  ;(call-with-output-file
    ;(build-path here "nscheme.scm.rkt")
    ;(lambda (out) (write (racket-datum nscheme.scm.rkt) out)))
  )
