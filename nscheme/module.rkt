#lang racket/base
(provide
  alist-ref
  alist-ref*
  module-apply
  parse/module
  eval/module
  link/module*
  )

(require
  "stage.rkt"
  "eval.rkt"
  "interop.rkt"
  racket/match
  )

(define (parse/module body)
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
         (vector required provided required-private provided-private body)))))

(define (stage/module body)
  (define mod (parse/module body))
  (define ($list _) (@lambda env:initial "x" "x"))
  (define (code env)
    (apply @lambda env (s->ns (vector-ref mod 2))
           (append (s->ns (vector-ref mod 4))
                   (list (cons $list (s->ns (vector-ref mod 3)))))))
  (vector (vector-ref mod 0) (vector-ref mod 1) (base:program code)))

(define (eval/module body)
  (define staged (stage/module body))
  (vector (vector-ref staged 0) (vector-ref staged 1)
          ($apply (ast-eval (vector-ref staged 2)) base:values)))

;(define (eval/module body)
  ;(define parsed (parse/module body))
  ;(define code `(lambda ,(vector-ref parsed 2) ,@(vector-ref parsed 4)
                  ;(,(lambda (env) (lambda (xs) xs)) . ,(vector-ref parsed 3))))
  ;(vector (vector-ref parsed 0) (vector-ref parsed 1)
          ;(eval env:base (s->ns code))))

(define (alist-ref alist k)
  (cdr (or (assoc k alist) (error "alist-ref of non-existent key:" k alist))))
(define (alist-ref* alist k*) (map (lambda (k) (alist-ref alist k)) k*))
(define (module-apply m env)
  (define imports (alist-ref* env (vector-ref m 0)))
  (map cons (vector-ref m 1) ((vector-ref m 2) imports)))
(define (link/module env m) (append (module-apply m env) env))
(define (link/module* env m*) (foldl (lambda (m e) (link/module e m)) env m*))
