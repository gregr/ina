#lang racket/base
(provide
  build-apply
  parse
  parse-lambda
  parse-let
  parse-let*
  parse-pair
  parse-thunk
  parse-unpair
  parse-value
  senv-new
  )

(require
  "term.rkt"
  gregr-misc/dict
  gregr-misc/either
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/monad
  gregr-misc/record
  gregr-misc/sugar
  racket/function
  racket/match
  )

(record senv mapping scope)
(define senv-empty (senv hash-empty 0))
(def (senv-get (senv mp _) name) (hash-get mp name))
(def (senv-add (senv mp scope) name)
  (senv (hash-set mp name scope) (+ 1 scope)))
(def (senv-add-anonymous (senv mp scope)) (senv mp (+ 1 scope)))
(def (senv-add-specials (senv mp scope) specials)
  (senv (dict-join mp specials) scope))
(define senv-new (curry senv-add-specials senv-empty))
(define (senv-lookup senv ident)
  (maybe->either (format "undefined identifier: ~s" ident)
                 (senv-get senv ident)))

(define (build-apply proc args)
  (foldl (lambda (arg proc) (t-apply proc arg)) proc args))

(def (build-lambda parse senv params body)
  senv = (forf senv = senv
               param <- params
               (senv-add senv param))
  (either-map (lambda (body) (forf body = body
                                   _ <- params
                                   (t-value (v-lam body))))
              (parse senv body)))

(define ((parse-thunk parse) senv stx)
  (either-map v-lam (parse (senv-add-anonymous senv) stx)))

(define (parse-identifier senv ident)
  (either-fold identity
    (match-lambda
      ((? integer? ri) (right (t-value (v-var (- (senv-scope senv) ri 1)))))
      (_ (left (format "invalid use of special identifier: ~s" ident))))
    (senv-lookup senv ident)))

(define ((parse parse-extra) senv stx)
  (define self (parse parse-extra))
  (define (parse-application senv head tail)
    (begin/with-monad either-monad
      proc <- (self senv head)
      args <- (monad-map either-monad (curry self senv) tail)
      (pure (build-apply proc args))))
  (match stx
    ((cons head tail)
     (match (if (symbol? head) (senv-get senv head) (nothing))
       ((just (? procedure? special)) (special senv head tail))
       (_ (parse-application senv head tail))))
    ('()         (right (t-value (v-unit))))
    ((? symbol?) (parse-identifier senv stx))
    (_           (parse-extra senv stx))))

(define ((parse-value parse) senv stx)
  (either-fold identity
               (match-lambda
                 ((t-value val) (right val))
                 (_ (left (format "invalid value syntax: ~s" stx))))
               (parse senv stx)))

(define ((parse-lambda parse) senv head tail)
  (match tail
    ((list (? non-empty-list? params) body)
     (build-lambda parse senv params body))
    (_ (left (format "invalid lambda: ~s" `(,head . ,tail))))))
(define ((parse-pair parse-value) senv head tail)
  (match tail
    ((list l r) (begin/with-monad either-monad
                                  vl <- (parse-value senv l)
                                  vr <- (parse-value senv r)
                                  (pure (t-value (v-pair vl vr)))))
    (_ (left (format "invalid pair: ~s" `(,head . ,tail))))))
(define ((parse-unpair parse) senv head tail)
  (match tail
    ((list bt pr) (begin/with-monad either-monad
                                    vb <- (parse senv bt)
                                    vp <- (parse senv pr)
                                    (pure (t-unpair vb vp))))
    (_ (left (format "invalid unpair: ~s" `(,head . ,tail))))))

(define (unzip-bindings err bindings)
  (begin/with-monad either-monad
    _ <- (for/list/monad either-monad ((binding bindings))
           (match binding ((list param arg) (right (void))) (_ (err))))
    (pure (zip-default '(() ()) bindings))))

(define ((parse-let parse) senv head tail)
  (define (err) (left (format "invalid let: ~s" `(,head . ,tail))))
  (match tail
    ((list (? non-empty-list? bindings) body)
     (begin/with-monad either-monad
                       (list params args) <- (unzip-bindings err bindings)
                       proc <- (build-lambda parse senv params body)
                       args <- (monad-map either-monad (curry parse senv) args)
                       (pure (build-apply proc args))))
    (_ (err))))
(define ((parse-let* parse) senv head tail)
  (define (err) (left (format "invalid let*: ~s" `(,head . ,tail))))
  (match tail
    ((list (? non-empty-list? bindings) body)
     (begin/with-monad either-monad
                       (list params args) <- (unzip-bindings err bindings)
                       (list senv parsed-args) <-
                       (for/fold/monad either-monad
                         (list senv parsed-args) (list senv '())
                         ((param params) (arg args))
                         parg <- (parse senv arg)
                         (pure (list (senv-add senv param)
                                     (list* parg parsed-args))))
                       body <- (parse senv body)
                       (pure (forf body = body
                                   arg <- parsed-args
                                   (t-apply (t-value (v-lam body)) arg)))))
    (_ (err))))
