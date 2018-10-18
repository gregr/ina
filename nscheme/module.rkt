#lang racket/base
(provide
  nscheme-module
  eval/module
  link/module*
  )

(require
  "stage.rkt"
  racket/match
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
                  `#(,(map cadr pd) ,(map car pd)) body))))))

(define (nscm:eval form) (eval (s->ns form)))
(define (eval/module m)
  (cons (map symbol->string (vector-ref (nsmod-required m) 0))
        (cdr (nscm:eval
               `(cons ',(vector-ref (nsmod-required m) 1)
                      (lambda ,(vector-ref (nsmod-required m) 1)
                        ,@(nsmod-body m)
                        (list . ,(map (lambda (enew e) `(cons ',enew ,e))
                                      (vector-ref (nsmod-provided m) 0)
                                      (vector-ref (nsmod-provided m) 1)))))))))

(define (import-apply i env)
  ((cdr i) (map (lambda (name)
                  (cdr (or (assoc name env)
                           (error "missing argument:" name)))) (car i))))
(define (link/module env m) (append (import-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))
