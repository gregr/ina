#lang racket/base
(provide
  read/provenance
  )
(require
  "datum.rkt"
  )

(define (read/provenance in)
  ;; TODO: gather real provenance while reading.
  (datum->datum/provenance (lambda (_) #f) (read in)))
