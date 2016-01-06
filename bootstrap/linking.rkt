#lang racket/base
(provide
  tuple0->list
  open-module
  link-module
  link-program
  )

(require
  "operation.rkt"
  "parsing.rkt"
  "substitution.rkt"
  "term.rkt"
  gregr-misc/list
  gregr-misc/sugar
  racket/match
  )

(module+ test
  (require rackunit))

(define (tuple0->list tup)
  (match tup
    ((annotated _ tup) (tuple0->list tup))
    ((v-unit)     '())
    ((v-pair l r) (list* l (tuple0->list r)))))

(define (open-module mod specs)
  (forl spec <- specs
        (match spec
          ((? symbol?)               (list spec (mod spec)))
          ((list name original-name) (list name (mod original-name)))
          (_ (error (format "invalid open-module spec: ~s" spec))))))

(def ((link-program parse) imports body)
  (list import-params import-args) = (zip-default '(() ()) imports)
  (if (null? import-params) (parse body)
    (build-apply (parse `(lambda ,import-params ,body)) import-args)))

; TODO: bindings must not redefine 'pair'; this can be fixed with more work
(def ((link-module parse) imports bindings)
  names = (append (map car imports) (map car bindings))
  body = (foldr (lambda (name acc) `(pair ,name ,acc)) '() names)
  prog = ((link-program parse) imports `(let* ,bindings ,body))
  vals = (match (substitute-full (step-complete prog))
           ((annotated _ vals) vals)
           (vals vals))
  vals = (tuple0->list (t-value-v vals))
  (make-immutable-hash (zip-with* cons names vals)))
