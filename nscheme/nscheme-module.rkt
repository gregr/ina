#lang racket/base
(provide
  nscheme-module
  eval/module
  link/module
  link/module*
  library-get
  )

(require
  racket/match
  racket/runtime-path
  (only-in "nscheme.rkt" import-apply)  ;; namespace-attach-module depends on this.
  )

(define (nsmod required provided body) (vector required provided body))
(define (nsmod-required m)             (vector-ref m 0))
(define (nsmod-provided m)             (vector-ref m 1))
(define (nsmod-body m)                 (vector-ref m 2))

(define local-ns (current-namespace))
(define-runtime-module-path nscheme-rkt "nscheme.rkt")
(define nscm-ns (parameterize ((current-namespace (make-base-namespace)))
                  (namespace-attach-module local-ns nscheme-rkt)
                  (namespace-require/constant nscheme-rkt)
                  (current-namespace)))

(define (nscm:eval form) (eval form nscm-ns))

(define (eval/module m)
  (cons (map symbol->string (vector-ref (nsmod-required m) 0))
        (cdr (nscm:eval
               `(import ,(vector-ref (nsmod-required m) 1)
                  ,@(nsmod-body m)
                  (map (lambda (enew e) (cons enew (cdr e)))
                       ',(vector-ref (nsmod-provided m) 0)
                       (export . ,(vector-ref (nsmod-provided m) 1))))))))

(define (nscheme-module body)
  (define (i->r items rrns)
    (for/fold ((rrns rrns)) ((item items))
      (match item
        (`(rename . ,rns) (append (reverse rns) rrns))
        (name (cons (list name name) rrns)))))
  (define (body-element def)
    (match def
      ((cons 'unquote body)          (nscm:eval `(let () . ,body)))
      ((cons 'unquote-splicing body) (cons 'begin
                                           (nscm:eval `(let () . ,body))))
      (_ def)))
  (let loop ((body body) (rrequired '()) (rprovided '()))
    (define next (and (pair? body) (car body)))
    (match next
      (`(require . ,items) (loop (cdr body) (i->r items rrequired) rprovided))
      (`(provide . ,items) (loop (cdr body) rrequired (i->r items rprovided)))
      (_ (let ((rd (reverse rrequired)) (pd (reverse rprovided)))
           (nsmod `#(,(map car rd) ,(map cadr rd))
                  `#(,(map cadr pd) ,(map car pd))
                  (map body-element body)))))))

(define (link/module env m) (append (import-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))

(define (assoc-get a k)
  (cdr (or (assoc k a) (error "assoc missing key:" k (map car a)))))
(define (assoc-map a f)
  (map (lambda (kv) (cons (car kv) (f (cdr kv)))) a))
(define (library-get a k) (assoc-map (assoc-get a k) eval/module))
