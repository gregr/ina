#lang racket/base
(require
  "../platform/racket/nscheme.rkt" "include.rkt"
  racket/file racket/include racket/pretty racket/runtime-path racket/splicing)
;(require profile)

;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

;;; This variant of bootstrapping stratifies evaluation so that values computed at different phases
;;; do not mix.  That means compilation does not need to support cross-stage persistence.

(let ()
  (define program (make-program))
  (define (link-definition* env def*) (program-link-definition* program env def*))
  (define env.primitive.privileged.all.0
    (env-conjoin env.primitive.privileged.control env.primitive.privileged))
  (define env.privileged.0
    (env-conjoin* env.minimal env.primitive env.primitive.privileged.all.0))
  (define env.unprivileged.0
    (env-conjoin env.minimal env.primitive))
  (define env.include/base/early
    (link-definition* env.privileged.0 def*.include/base/early))
  (define env.include/base/early.0
    (env-conjoin env.privileged.0 env.include/base/early))
  (define env.include/boot
    (link-definition* env.include/base/early.0 def*.include/boot))
  (define env.include/boot.0
    (env-conjoin env.include/base/early.0 env.include/boot))
  (define env.include/base
    (env-conjoin env.include/base/early (link-definition* env.include/boot.0 def*.include/base)))
  (define env.include/base.0
    (env-conjoin env.unprivileged.0 env.include/base))
  (define env.include
    (env-conjoin env.include/base (link-definition* env.include/base.0 def*.include)))
  (define env.include.0
    (env-conjoin env.unprivileged.0 env.include))
  (define env.eval
    (link-definition* env.include.0 def*.eval))
  (define env.test
    (env-conjoin env.include.0 env.eval))
  (define stx*.test (list '(list
                            (+ 1 2)
                            (foldr + 0 '(1 2 3 4 5))
                            (foldr cons '(1 2) '(3 4 5))
                            (foldl cons '(1 2) '(3 4 5))
                            (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                       '(1 2) '(3 4 5) '(a b c))
                            (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                        '(1 2) '(3 4 5) '(a b c)))))
  (void (link-definition* env.test stx*.test))
  (displayln "parsing test:")
  ;; ~30ms
  (define E.test (time (program->E program)))
  (displayln "evaluating test:")
  ;; ~5ms
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
    (define program (make-program))
    (define (link-definition* env def*) (program-link-definition* program env def*))
    (define env.primitive.privileged.all.0
      (env-conjoin env.primitive.privileged.control env.primitive.privileged))
    (define env.privileged.0
      (env-conjoin* env.minimal env.primitive env.primitive.privileged.all.0))
    (define env.unprivileged.0
      (env-conjoin env.minimal env.primitive))
    (define env.include/base/early
      (link-definition* env.privileged.0 def*.include/base/early))
    (define env.include/base/early.0
      (env-conjoin env.privileged.0 env.include/base/early))
    (define env.include/boot
      (link-definition* env.include/base/early.0 def*.include/boot))
    (define env.include/boot.0
      (env-conjoin env.include/base/early.0 env.include/boot))
    (define env.include/base
      (env-conjoin env.include/base/early (link-definition* env.include/boot.0 def*.include/base)))
    (define env.include/base.0
      (env-conjoin env.unprivileged.0 env.include/base))
    (define env.include
      (env-conjoin env.include/base (link-definition* env.include/base.0 def*.include)))
    (define env.include.0
      (env-conjoin env.unprivileged.0 env.include))
    (define env.eval
      (link-definition* env.include.0 def*.eval))
    (define env.test
      (env-conjoin env.include.0 env.eval))
    (define stx*.test (list '(list
                              (+ 1 2)
                              (foldr + 0 '(1 2 3 4 5))
                              (foldr cons '(1 2) '(3 4 5))
                              (foldl cons '(1 2) '(3 4 5))
                              (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                         '(1 2) '(3 4 5) '(a b c))
                              (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                          '(1 2) '(3 4 5) '(a b c)))))
    (void (link-definition* env.test stx*.test))
    ;;;;;;;;;;;;;;
    ;; End copy ;;
    ;;;;;;;;;;;;;;
    (program->E program)))
(displayln "parsing self-apply1:")
;; ~40ms
(define E.self-apply1 (time (parse-body env.large stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.large stx*.self-apply1)))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~2100ms
(define E.self-apply2 (time (ns-eval E.self-apply1)))
;(define E.self-apply2 (profile (ns-eval E.self-apply1)))
(displayln "evaluating self-apply2:")
;; ~5ms
(pretty-write (time (ns-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
