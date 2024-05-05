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
  ;(pretty-write (env-describe env.include))
  (displayln "parsing test:")
  ;; ~0ms
  (define E.test (time (parse-body env.include stx*.test)))
  ;(define E.test (profile (parse-body env.include stx*.test)))
  ;(pretty-write E.test)
  ;(pretty-write (E-pretty E.test))
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
    (define env.primitive.privileged.all
      (env-conjoin env.primitive.privileged.control env.primitive.privileged))
    (define env.include/boot
      (env-conjoin* env.minimal env.primitive
                    (eval-definition*
                     (env-conjoin* env.minimal env.primitive env.primitive.privileged.all)
                     def*.include/boot)))
    (define env.include/base
      (env-conjoin (eval-definition* env.include/boot def*.include/base) env.include/boot))
    (define env.include.0
      (env-conjoin (eval-definition* env.include/base def*.include) env.include/base))
    (define env.include
      (env-conjoin (eval-definition* (env-conjoin env.include.0 env.primitive.privileged.all)
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
    ;;;;;;;;;;;;;;
    ;; End copy ;;
    ;;;;;;;;;;;;;;
    (parse-body env.include stx*.test)))
(displayln "parsing self-apply1:")
;; ~1ms
(define E.self-apply1 (time (parse-body env.include.extended stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.include.extended stx*.self-apply1)))
;(pretty-write E.self-apply1)
;(pretty-write (E-pretty E.self-apply1))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~4550ms
(define E.self-apply2 (time (ns-eval E.self-apply1)))

;(define E.self-apply2 (profile (ns-eval E.self-apply1)))
;(pretty-write (E-pretty E.self-apply2))
(displayln "evaluating self-apply2:")
;; ~3ms
(pretty-write (time (ns-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
