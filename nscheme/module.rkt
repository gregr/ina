#lang racket/base
(provide
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

(define (import-apply i env)
  (define (ref name)
    (cdr (or (assoc name env) (error "missing argument:" name))))
  (map cons (vector-ref i 1) ((vector-ref i 2) (map ref (vector-ref i 0)))))
(define (link/module env m) (append (import-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))
