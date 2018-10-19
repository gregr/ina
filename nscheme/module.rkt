#lang racket/base
(provide
  alist-ref*
  module-apply
  eval/module
  link/module*
  )

(require
  "stage.rkt"
  racket/match
  )

(define (eval/module body)
  (define (i->r items rrns) (for/fold ((rrns rrns)) ((item items))
                              (match item
                                (`(rename . ,rns) (append (reverse rns) rrns))
                                (name (cons (list name name) rrns)))))
  (let loop ((body body) (rrequired '()) (rprovided '()))
    (define next (and (pair? body) (car body)))
    (match next
      (`(require . ,items) (loop (cdr body) (i->r items rrequired) rprovided))
      (`(provide . ,items) (loop (cdr body) rrequired (i->r items rprovided)))
      (_ (define rd (reverse rrequired)) (define pd (reverse rprovided))
         (define required (map car rd)) (define required-private (map cadr rd))
         (define provided (map cadr pd)) (define provided-private (map car pd))
         (vector required provided
                 (eval (s->ns `(lambda ,required-private
                                 ,@body (list . ,provided-private)))))))))

(define (alist-ref* alist k*)
  (map (lambda (k) (cdr (or (assoc k alist) (error "missing key:" k)))) k*))
(define (module-apply m env)
  (define imports (alist-ref* env (vector-ref m 0)))
  (map cons (vector-ref m 1) ((vector-ref m 2) imports)))
(define (link/module env m) (append (module-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))
