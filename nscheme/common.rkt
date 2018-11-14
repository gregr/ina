#lang racket/base
(provide
  (all-from-out "interop.rkt")
  (rename-out (nscm-equal? equal?) (nscm-member member) (nscm-assoc assoc))
  (rename-out (nscm-quote quote) (nscm-quasiquote quasiquote)))

(require "interop.rkt")
