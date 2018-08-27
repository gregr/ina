#lang racket/base
(provide
  nscheme-module
  eval/module
  apply/module
  link/module
  )

(require
  racket/match
  racket/runtime-path
  (only-in "nscheme.rkt")  ;; namespace-attach-module depends on this.
  )

(define (nsmod required provided body) (vector required provided body))
(define (nsmod-required m)             (vector-ref m 0))
(define (nsmod-provided m)             (vector-ref m 1))
(define (nsmod-body m)                 (vector-ref m 2))

(define (nscheme-module body)
  (define (i->r items rrns)
    (for/fold ((rrns rrns)) ((item items))
      (match item
        (`(rename . ,rns) (append (reverse rns) rrns))
        (name (cons (list name name) rrns)))))
  (let loop ((body body) (rrequired '()) (rprovided '()))
    (define next (and (pair? body) (car body)))
    (match next
      (`(require . ,items) (loop (cdr body) (i->r items rrequired) rprovided))
      (`(provide . ,items) (loop (cdr body) rrequired (i->r items rprovided)))
      (_ (let ((rd (reverse rrequired)) (pd (reverse rprovided)))
           (nsmod `#(,(map car rd) ,(map cadr rd))
                  `#(,(map cadr pd) ,(map car pd))
                  body))))))

(define local-ns (current-namespace))
(define-runtime-module-path nscheme-rkt "nscheme.rkt")
(define nscm-ns (parameterize ((current-namespace (make-base-namespace)))
                  (namespace-attach-module local-ns nscheme-rkt)
                  (namespace-require/constant nscheme-rkt)
                  (current-namespace)))

(define (eval/module m)
  (cons (vector-ref (nsmod-required m) 0)
        (eval `(lambda ,(vector-ref (nsmod-required m) 1)
                 ,@(nsmod-body m)
                 (list . ,(map (lambda (enew eold) `(cons ',enew ,eold))
                               (vector-ref (nsmod-provided m) 0)
                               (vector-ref (nsmod-provided m) 1)))) nscm-ns)))

(define (apply/module m env)
  (define rs (map (lambda (r) (cdr (or (assoc (symbol->string r) env)
                                       (error "missing requirement:" r))))
                  (car m)))
  (apply (cdr m) rs))

(define (link/module m env) (append (apply/module m env) env))
