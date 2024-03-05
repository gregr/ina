#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt"
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
  (pretty-write (time (E-eval E.test)))
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
      (env-compose env.primitive.privileged env.primitive.privileged.control))
    (define env.include/boot
      (env-compose* (eval-definition*
                      (env-compose* env.primitive.privileged.all env.primitive env.minimal)
                      def*.include/boot)
                    env.primitive
                    env.minimal))
    (define env.include/base
      (env-compose env.include/boot (eval-definition* env.include/boot def*.include/base)))
    (define env.include.0
      (env-compose env.include/base (eval-definition* env.include/base def*.include)))
    (define env.include
      (env-compose env.include.0
                   (eval-definition* (env-compose env.primitive.privileged.all env.include.0)
                                     def*.primitive)))
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
(define E.self-apply2 (time (E-eval E.self-apply1)))
;(define E.self-apply2 (profile (E-eval E.self-apply1)))
;(pretty-write (E-pretty E.self-apply2))
(displayln "evaluating self-apply2:")
;; ~3ms
(pretty-write (time (E-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
