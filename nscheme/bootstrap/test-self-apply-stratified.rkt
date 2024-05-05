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
  (define env.primitive.privileged.all
    (env-conjoin env.primitive.privileged.control env.primitive.privileged))
  (define env.include/boot
    (env-conjoin* env.minimal env.primitive
                  (link-definition*
                   (env-conjoin* env.minimal env.primitive env.primitive.privileged.all)
                   def*.include/boot)))
  (define env.include/base
    (env-conjoin (link-definition* env.include/boot def*.include/base) env.include/boot))
  (define env.include.0
    (env-conjoin (link-definition* env.include/base def*.include) env.include/base))
  (define env.include
    (env-conjoin (link-definition* (env-conjoin env.include.0 env.primitive.privileged.all)
                                   def*.primitive)
                 env.include.0))
  (define stx*.test (list '(list
                            (+ 1 2)
                            (foldr + 0 '(1 2 3 4 5))
                            (foldr cons '(1 2) '(3 4 5))
                            (foldl cons '(1 2) '(3 4 5))
                            (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                       '(1 2) '(3 4 5) '(a b c))
                            (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                        '(1 2) '(3 4 5) '(a b c)))))
  (void (link-definition* env.include stx*.test))
  (displayln "parsing test:")
  ;; ~0ms
  (define E.test (time (program->E program)))
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
  `((define def*.include/boot ',def*.include/boot)
    (define def*.include/base ',def*.include/base)
    (define def*.include      ',def*.include)
    (define def*.primitive    ',def*.primitive)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Begin copy of earlier definitions ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define program (make-program))
    (define (link-definition* env def*) (program-link-definition* program env def*))
    (define env.primitive.privileged.all
      (env-conjoin env.primitive.privileged.control env.primitive.privileged))
    (define env.include/boot
      (env-conjoin* env.minimal env.primitive
                    (link-definition*
                     (env-conjoin* env.minimal env.primitive env.primitive.privileged.all)
                     def*.include/boot)))
    (define env.include/base
      (env-conjoin (link-definition* env.include/boot def*.include/base) env.include/boot))
    (define env.include.0
      (env-conjoin (link-definition* env.include/base def*.include) env.include/base))
    (define env.include
      (env-conjoin (link-definition* (env-conjoin env.include.0 env.primitive.privileged.all)
                                     def*.primitive)
                   env.include.0))
    (define stx*.test (list '(list
                              (+ 1 2)
                              (foldr + 0 '(1 2 3 4 5))
                              (foldr cons '(1 2) '(3 4 5))
                              (foldl cons '(1 2) '(3 4 5))
                              (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                         '(1 2) '(3 4 5) '(a b c))
                              (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                          '(1 2) '(3 4 5) '(a b c)))))
    (void (link-definition* env.include stx*.test))
    ;;;;;;;;;;;;;;
    ;; End copy ;;
    ;;;;;;;;;;;;;;
    (program->E program)))
(displayln "parsing self-apply1:")
;; ~1ms
(define E.self-apply1 (time (parse-body env.include.extended stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.include.extended stx*.self-apply1)))
;(pretty-write (E-pretty E.self-apply1))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~3200ms
(define E.self-apply2 (time (ns-eval E.self-apply1)))
;(define E.self-apply2 (profile (ns-eval E.self-apply1)))
;(pretty-write (E-pretty E.self-apply2))
(displayln "evaluating self-apply2:")
;; ~14ms
(pretty-write (time (ns-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
