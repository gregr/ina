#lang racket/base
(require
  "../platform/racket/nscheme.rkt" "include.rkt"
  racket/file racket/include racket/pretty racket/runtime-path racket/splicing)
;(require profile)

;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

;;; This variant of bootstrapping is unstratified, mixing values computed at different phases.  That
;;; means compilation must support cross-stage persistence.

(let ()
  (define stx*.test (list '(list
                            (+ 1 2)
                            (foldr + 0 '(1 2 3 4 5))
                            (foldr cons '(1 2) '(3 4 5))
                            (foldl cons '(1 2) '(3 4 5))
                            (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                       '(1 2) '(3 4 5) '(a b c))
                            (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                        '(1 2) '(3 4 5) '(a b c)))))
  (displayln "parsing test:")
  ;; ~2ms
  (define E.test (time (parse-body env.large stx*.test)))
  (displayln "evaluating test:")
  ;; ~0ms
  (pretty-write (time (ns-eval E.test)))
  ;==>
  ;(3
  ; 15
  ; (3 4 5 1 2)
  ; (5 4 3 1 2)
  ; ((5 . c) (4 . b) (3 . a) 1 2)
  ; ((3 . a) (4 . b) (5 . c) 1 2))
  )

(define stx*.self-apply1
  `((define def*.include/base/early     ',def*.include/base/early)
    (define def*.include/boot           ',def*.include/boot)
    (define def*.include/base           ',def*.include/base)
    (define def*.include                ',def*.include)
    (define def*.eval                   ',def*.eval)
    (define def*.primitive-environments ',def*.primitive-environments)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Begin copy of earlier definitions ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define env.primitive.privileged.all.0
      (env-conjoin env.primitive.privileged.control env.primitive.privileged))
    (define env.privileged.0
      (env-conjoin* env.minimal env.primitive env.primitive.privileged.all.0))
    (define env.unprivileged.0
      (env-conjoin env.minimal env.primitive))
    (define env.include/base/early
      (eval-definition* env.privileged.0 def*.include/base/early))
    (define env.include/base/early.0
      (env-conjoin env.privileged.0 env.include/base/early))
    (define env.include/boot
      (eval-definition* env.include/base/early.0 def*.include/boot))
    (define env.include/boot.0
      (env-conjoin env.include/base/early.0 env.include/boot))
    (define env.include/base
      (env-conjoin env.include/base/early (eval-definition* env.include/boot.0 def*.include/base)))
    (define env.include/base.0
      (env-conjoin env.unprivileged.0 env.include/base))
    (define env.include
      (env-conjoin env.include/base (eval-definition* env.include/base.0 def*.include)))
    (define env.include.0
      (env-conjoin env.unprivileged.0 env.include))
    (define env.eval
      (eval-definition* env.include.0 def*.eval))
    (define env.primitive-environments
      (eval-definition* (env-conjoin env.include.0 env.primitive.privileged.all.0)
                        def*.primitive-environments))
    (define env.minimal.1
      (E-eval (parse-expression env.include 'env.minimal)))
    (define env.primitive.1
      (E-eval (parse-expression env.primitive-environments 'env.primitive)))
    (define env.primitive.privileged.1
      (E-eval (parse-expression env.primitive-environments 'env.primitive.privileged)))
    (define env.primitive.privileged.control.1
      (E-eval (parse-expression env.primitive-environments 'env.primitive.privileged.control)))
    (define env.primitive.privileged.all.1
      (env-conjoin env.primitive.privileged.control.1 env.primitive.privileged.1))
    (define env.unprivileged.1
      (env-conjoin env.minimal.1 env.primitive.1))
    (define env.include/base.1
      (env-conjoin env.unprivileged.1 env.include/base))
    (define env.include.1
      (env-conjoin env.unprivileged.1 env.include))
    (define env.extended.1
      (E-eval (parse-expression env.eval 'env.extended)))
    (define env.large
      (env-conjoin* env.extended.1 env.eval env.include.1 env.primitive-environments))
    (define stx*.test (list '(list
                              (+ 1 2)
                              (foldr + 0 '(1 2 3 4 5))
                              (foldr cons '(1 2) '(3 4 5))
                              (foldl cons '(1 2) '(3 4 5))
                              (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                         '(1 2) '(3 4 5) '(a b c))
                              (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                          '(1 2) '(3 4 5) '(a b c)))))
    ;;;;;;;;;;;;;;
    ;; End copy ;;
    ;;;;;;;;;;;;;;
    (parse-body env.include.1 stx*.test)))
(displayln "parsing self-apply1:")
;; ~40ms
;(define E.self-apply1 (time (parse-body env.include.extended stx*.self-apply1)))
(define E.self-apply1 (time (parse-body env.large stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.include.extended stx*.self-apply1)))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~3000ms
(define E.self-apply2 (time (ns-eval E.self-apply1)))
;(define E.self-apply2 (profile (ns-eval E.self-apply1)))
(displayln "evaluating self-apply2:")
;; ~1ms
(pretty-write (time (ns-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
