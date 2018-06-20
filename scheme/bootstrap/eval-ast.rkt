#lang racket/base
(provide
  eval-ast
  closure->procedure
  )

(require
  "ast.rkt"
  "data.rkt"
  "expand.rkt"
  "match.rkt"
  racket/control
  racket/list
  )

;;; Runtime environments
(define env-empty (hash))
(define (env-extend* env b*)
  (foldl (lambda (b e) (hash-set e (car b) (box (cdr b)))) env b*))
(define (env-ref-box env addr)
  (hash-ref env addr (lambda () (error "unbound address:" addr))))
(define (env-ref env addr) (unbox (env-ref-box env addr)))
(define (env-set! env addr v) (set-box! (env-ref-box env addr) v))

;;; Primitive operations
(define primitive-op-evaluators
  (make-immutable-hash
    (map (lambda (po-desc)
           (define name (car po-desc))
           (define arg-sig (cadr po-desc))
           (define op (caddr po-desc))
           (define (valid? a*)
             (andmap (lambda (ty? a) (or (not ty?) (ty? a))) arg-sig a*))
           (define (full-op a*)
             (if (valid? a*) (apply op a*)
               (error "primitive op type error:" name arg-sig a*)))
           (cons name full-op)) primitive-ops)))

;;; Evaluation
(define (closure->procedure clo)
  (define addr* (closure-param* clo))
  (define cenv (closure-env clo))
  (define body (closure-body clo))
  (define (continue a*)
    (define b?* (map (lambda (addr a) (and addr (cons addr a))) addr* a*))
    (define env (env-extend* cenv (filter-not not b?*)))
    (eval-ast/env env body))
  (define plen (length addr*))
  (cond ((closure-variadic? clo)
         (lambda arg*
           (when (< (length arg*) (- plen 1))
             (error "too few arguments:" (- plen 1) (length arg*) clo arg*))
           (let ((a0* (take arg* (- plen 1))) (a1* (drop arg* (- plen 1))))
             (continue (append a0* (list a1*))))))
        (else (lambda arg*
                (when (not (= (length arg*) plen))
                  (error "arity mismatch:" plen (length arg*) clo arg*))
                (continue arg*)))))

(define (evaluate-apply proc arg*)
  (apply (cond ((procedure? proc) proc)
               ((closure? proc) (closure->procedure proc))
               (else (error "cannot apply non-procedure:" proc))) arg*))

(define (eval-ast/env env tm)
  (define (loop tm) (eval-ast/env env tm))
  (match (ast->v tm)
    (`#(quote ,datum)         datum)
    (`#(var ,address)         (env-ref env address))
    (`#(set! ,address ,tm)    (env-set! env address (loop tm)))
    (`#(if ,c ,t ,f)          (if (loop c) (loop t) (loop f)))
    ;; To produce racket procedures, use this line instead:
    ;(`#(lambda ,v? ,a* ,body) (closure->procedure (closure v? a* body env)))
    (`#(lambda ,v? ,a* ,body) (closure v? a* body env))
    (`#(apply* ,p ,a*)        (evaluate-apply (loop p) (map loop a*)))
    (`#(apply ,p ,a)          (define a* (loop a))
                              (when (not (list? a*))
                                (error "invalid argument list:" a a*))
                              (evaluate-apply (loop p) a*))

    (`#(prim-op ,name ,a*)
      (define op (hash-ref primitive-op-evaluators name))
      (op (map loop a*)))

    (`#(reset ,body) (reset (loop body)))
    (`#(shift ,proc)
      (shift k (evaluate-apply (loop proc) (list (continuation k)))))
    (`#(unshift ,k-raw ,arg)
      (define k (loop k-raw))
      (cond ((continuation? k) ((continuation-k k) (loop arg)))
            (else (error "invalid continuation:" k))))

    (_ (error "unknown term:" (ast->v tm)))))

(define env-initial
  (env-extend*
    env-empty (map (lambda (b) (cons (car b) (eval-ast/env env-empty (cdr b))))
                   primitive-op-module)))
(define (eval-ast tm) (eval-ast/env env-initial tm))
