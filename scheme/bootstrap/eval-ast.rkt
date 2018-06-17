#lang racket/base
(provide
  eval-ast
  )

(require
  "ast.rkt"
  "data.rkt"
  "match.rkt"
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
(define (evaluate-apply proc arg*)
  (when (not (closure? proc)) (error "cannot apply non-procedure:" proc))
  (define addr* (closure-param* proc))
  (define (continue a*)
    (define b?* (map (lambda (addr a) (and addr (cons addr a))) addr* a*))
    (define env (env-extend* (closure-env proc) (filter-not not b?*)))
    (eval-ast/env env (closure-body proc)))
  (define plen (length addr*)) (define alen (length arg*))
  (cond ((and (closure-variadic? proc) (<= (- plen 1) alen))
         (let ((a0* (take arg* (- plen 1))) (a1* (drop arg* (- plen 1))))
           (continue (append a0* (list a1*)))))
        ((= plen alen) (continue arg*))
        (else (error "argument count mismatch:" plen alen proc arg*))))

(define (eval-ast/env env tm)
  (define (loop tm) (eval-ast/env env tm))
  (match (ast->v tm)
    (`#(quote ,datum)         datum)
    (`#(var ,address)         (env-ref env address))
    (`#(set! ,address ,tm)    (env-set! env address (loop tm)))
    (`#(if ,c ,t ,f)          (if (loop c) (loop t) (loop f)))
    (`#(lambda ,v? ,a* ,body) (closure v? a* body env))
    (`#(apply* ,p ,a*)        (evaluate-apply (loop p) (map loop a*)))
    (`#(apply ,p ,a)          (define a* (loop a))
                              (when (not (list? a*))
                                (error "invalid argument list:" a a*))
                              (evaluate-apply (loop p) a*))

    (`#(prim-op ,name ,a*)
      (define op (hash-ref primitive-op-evaluators name))
      (op (map loop a*)))

    ;; TODO: translate these.
    ;(`#(reset ,tbody) (reset (evaluate depth env tbody)))

    ;(`#(shift ,tproc)
      ;(define proc (evaluate depth env tproc))
      ;(shift k (evaluate-apply proc (vector (continuation k)))))

    ;(`#(unshift ,tk ,targ)
      ;(define k (evaluate depth env tk))
      ;(define arg (evaluate depth env targ))
      ;(cond ((continuation? k) ((continuation-k k) arg))
            ;(else (exception `(unshift-failed: ,k) tm))))

    (_ (error "unknown term:" (ast->v tm)))))

(define (eval-ast tm) (eval-ast/env env-empty tm))
