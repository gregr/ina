#lang racket/base
(provide
  build-apply
  error-parse
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
  gregr-misc/list
  gregr-misc/maybe
  gregr-misc/record
  gregr-misc/sugar
  racket/format
  racket/function
  racket/match
  )

;(define context->string #f)
(define context->string ~s)
(define current-context (make-parameter #f))
(define (error-parse msg)
  (define context (current-context))
  (error (if (and context context->string)
           (format "~a; ~a" msg (context->string context)) msg)))

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
  (match (senv-get senv ident)
    ((nothing) (error-parse "undefined identifier"))
    ((just val) val)))

(define (build-apply proc args)
  (foldl (lambda (arg proc) (t-apply proc arg)) proc args))

(def (build-lambda parse senv params body)
  senv = (forf senv = senv
               param <- params
               (senv-add senv param))
  (forf body = (parse senv body)
        _ <- params
        (t-value (v-lam body))))

(define ((parse-thunk parse) senv stx)
  (v-lam (parse (senv-add-anonymous senv) stx)))

(define (parse-identifier senv ident)
  (match (senv-lookup senv ident)
    ((? integer? ridx) (t-value (v-var (- (senv-scope senv) ridx 1))))
    (_ (error-parse "invalid use of special identifier"))))

(define ((parse parse-extra) senv stx)
  (define (self senv stx)
    (parameterize ((current-context stx))
      (match stx
        ((cons head tail)
         (match (if (symbol? head) (senv-get senv head) (nothing))
           ((just (? procedure? special)) (special senv tail))
           (_ (build-apply (self senv head) (map (curry self senv) tail)))))
        ('()         (t-value (v-unit)))
        ((? symbol?) (parse-identifier senv stx))
        (_           (parse-extra senv stx)))))
  (self senv stx))

(define ((parse-value parse) senv stx)
  (match (parse senv stx)
    ((t-value val) val)
    (_ (error-parse "invalid value syntax"))))

(define ((parse-lambda parse) senv tail)
  (match tail
    ((list (? non-empty-list? params) body)
     (build-lambda parse senv params body))
    (_ (error-parse "invalid lambda"))))
(define ((parse-pair parse-value) senv tail)
  (match tail
    ((list l r) (t-value (apply v-pair (map (curry parse-value senv) tail))))
    (_ (error-parse "invalid pair"))))
(define ((parse-unpair parse) senv tail)
  (match tail
    ((list bt pr) (t-unpair (parse senv bt) (parse senv pr)))
    (_ (error-parse "invalid unpair"))))

(def (unzip-bindings err bindings)
  (values params args) =
  (forf params = '() args = '()
        binding <- bindings
        (match binding
          ((list param arg) (values (list* param params)
                                    (list* arg args)))
          (_ (err))))
  (values (reverse params) (reverse args)))

(define ((parse-let parse) senv tail)
  (define (err) (error-parse "invalid let"))
  (match tail
    ((list (? non-empty-list? bindings) body)
     (lets (values params args) = (unzip-bindings err bindings)
           proc = (build-lambda parse senv params body)
           (build-apply proc (map (curry parse senv) args))))
    (_ (err))))
(define ((parse-let* parse) senv tail)
  (define (err) (error-parse "invalid let*"))
  (match tail
    ((list (? non-empty-list? bindings) body)
     (lets (values params args) = (unzip-bindings err bindings)
           (values senv parsed-args) =
           (forf senv = senv parsed-args = '()
                 param <- params
                 arg <- args
                 (values (senv-add senv param)
                         (list* (parse senv arg) parsed-args)))
           (forf body = (parse senv body)
                 arg <- parsed-args
                 (t-apply (t-value (v-lam body)) arg))))
    (_ (err))))
