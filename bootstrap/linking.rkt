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
  gregr-misc/either
  gregr-misc/list
  gregr-misc/monad
  gregr-misc/sugar
  racket/match
  )

(module+ test
  (require rackunit))

(define (tuple0->list tup)
  (match tup
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
    (either-map (lambda (pbody) (build-apply pbody import-args))
                (parse `(lambda ,import-params ,body)))))

; TODO: bindings must not redefine 'pair'; this can be fixed with more work
(define ((link-module parse) imports bindings)
  (begin/with-monad either-monad
    names = (append (map car imports) (map car bindings))
    body = (foldr (lambda (name acc) `(pair ,name ,acc)) '() names)
    prog <- ((link-program parse) imports `(let* ,bindings ,body))
    vals = (tuple0->list (t-value-v (substitute-full (step-complete prog))))
    (pure (make-immutable-hash (zip-with* cons names vals)))))
