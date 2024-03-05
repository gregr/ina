#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt"
         racket/file racket/include racket/pretty racket/runtime-path racket/splicing)
;(require profile)

;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

;;; This variant of bootstrapping stratifies evaluation so that values computed at different phases
;;; do not mix.  That means compilation does not need to support cross-stage persistence.

(define &D.program (box ($d:begin)))
(define (link-definition* env def*)
  (let ((env.d (make-env)))
    (set-box! &D.program ($d:begin (unbox &D.program)
                                   (parse-begin-definition* env.d (env-compose env env.d) def*)))
    env.d))

(let ()
  (define env.primitive.privileged.all
    (env-compose env.primitive.privileged env.primitive.privileged.control))

  (define env.include/boot
    (env-compose* (link-definition*
                    (env-compose* env.primitive.privileged.all env.primitive env.minimal)
                    def*.include/boot)
                  env.primitive
                  env.minimal))
  (define env.include/base
    (env-compose env.include/boot (link-definition* env.include/boot def*.include/base)))
  (define env.include.0
    (env-compose env.include/base (link-definition* env.include/base def*.include)))
  (define env.include
    (env-compose env.include.0
                 (link-definition* (env-compose env.primitive.privileged.all env.include.0)
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
  ;(pretty-write (env-describe env.include))
  (void (link-definition* env.include stx*.test))
  (displayln "parsing test:")
  ;; ~0ms
  (define E.test (time (D->E (unbox &D.program))))
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
    (define &D.program (box ($d:begin)))
    (define (link-definition* env def*)
      (let ((env.d (make-env)))
        (set-box! &D.program ($d:begin (unbox &D.program)
                                       (parse-begin-definition* env.d (env-compose env env.d) def*)))
        env.d))
    (define env.primitive.privileged.all
      (env-compose env.primitive.privileged env.primitive.privileged.control))
    (define env.include/boot
      (env-compose* (link-definition*
                      (env-compose* env.primitive.privileged.all env.primitive env.minimal)
                      def*.include/boot)
                    env.primitive
                    env.minimal))
    (define env.include/base
      (env-compose env.include/boot (link-definition* env.include/boot def*.include/base)))
    (define env.include.0
      (env-compose env.include/base (link-definition* env.include/base def*.include)))
    (define env.include
      (env-compose env.include.0
                   (link-definition* (env-compose env.primitive.privileged.all env.include.0)
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
    (void (link-definition* env.include stx*.test))
    ;;;;;;;;;;;;;;
    ;; End copy ;;
    ;;;;;;;;;;;;;;
    (D->E (unbox &D.program))))
(displayln "parsing self-apply1:")
;; ~1ms
(define E.self-apply1 (time (parse-body env.include.extended stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.include.extended stx*.self-apply1)))
;(pretty-write (E-pretty E.self-apply1))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~3200ms
(define E.self-apply2 (time (E-eval E.self-apply1)))
;(define E.self-apply2 (profile (E-eval E.self-apply1)))
;(pretty-write (E-pretty E.self-apply2))
(displayln "evaluating self-apply2:")
;; ~14ms
(pretty-write (time (E-eval E.self-apply2)))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
