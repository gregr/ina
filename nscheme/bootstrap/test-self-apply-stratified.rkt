#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt" racket/include)
(include "../include/platform/env/primitive.scm")
(include "../include/platform/posix/include.scm")
(include "../include/platform/env/evaluated.scm")
(include "../include/platform/posix/env/primitive.scm")
(include "../include/platform/posix/env/evaluated.scm")
;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

;;; This variant of bootstrapping stratifies evaluation so that values computed at different phases
;;; do not mix.  That means compilation does not need to support cross-stage persistence.

(define (with-time thunk) (with-milliseconds displayln thunk))

(let ()
  (define program (make-program))
  (define (link-definition* env def*) (program-parse-definition* program env def*))
  (define env.tiny             (env-conjoin* env.minimal env.common))
  (define env.small            (env-conjoin* env.tiny env.control env.io))
  (define env.small+privileged (env-conjoin* env.small env.privileged))
  (define env.base             (env-conjoin* env.small (link-definition* env.small+privileged def*.base)))
  (define env.compiler         (link-definition* env.base def*.compiler))
  (define env.nscheme          (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                                 (env-conjoin* env.deps env.meta (link-definition* env.deps def*.nscheme))))
  (define env.large            env.nscheme)
  (define stx*.test (list '(list
                            (+ 1 2)
                            (foldr + 0 '(1 2 3 4 5))
                            (foldr cons '(1 2) '(3 4 5))
                            (foldl cons '(1 2) '(3 4 5))
                            (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                       '(1 2) '(3 4 5) '(a b c))
                            (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                        '(1 2) '(3 4 5) '(a b c)))))
  (void (link-definition* env.large stx*.test))
  (displayln "parsing test:")
  ;; ~30ms
  (define E.test (with-time (lambda () (program->E program))))
  (displayln "evaluating test:")
  ;; ~5ms
  (pretty-write (with-time (lambda () (E-eval E.test))))
  ;==>
  ;(3
  ; 15
  ; (3 4 5 1 2)
  ; (5 4 3 1 2)
  ; ((5 . c) (4 . b) (3 . a) 1 2)
  ; ((3 . a) (4 . b) (5 . c) 1 2))
  )

(define stx*.self-apply1
  (append
   `((define def*.base     ',def*.base)
     (define def*.compiler ',def*.compiler)
     (define def*.nscheme  ',def*.nscheme)
     (define def*.posix    ',def*.posix))
   def*.primitive
   def*.syntax
   (file-name->stx* "../include/platform/env/primitive.scm")
   (file-name->stx* "../include/platform/posix/env/primitive.scm")
   '((define program (make-program))
     (define (link-definition* env def*) (program-parse-definition* program env def*))
     (define env.tiny             (env-conjoin* env.minimal env.common))
     (define env.small            (env-conjoin* env.tiny env.control env.io))
     (define env.small+privileged (env-conjoin* env.small env.privileged))
     (define env.base             (env-conjoin* env.small (link-definition* env.small+privileged def*.base)))
     (define env.compiler         (link-definition* env.base def*.compiler))
     (define env.nscheme          (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                                    (env-conjoin* env.deps env.meta (link-definition* env.deps def*.nscheme))))
     (define env.large            env.nscheme))
   '((define stx*.test (list '(list
                               (+ 1 2)
                               (foldr + 0 '(1 2 3 4 5))
                               (foldr cons '(1 2) '(3 4 5))
                               (foldl cons '(1 2) '(3 4 5))
                               (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                          '(1 2) '(3 4 5) '(a b c))
                               (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                           '(1 2) '(3 4 5) '(a b c)))))
     (void (link-definition* env.large stx*.test))
     (program->E program))))
(displayln "parsing self-apply1:")
;; ~2ms
(define E.self-apply1 (with-time (lambda () (parse-body env.large+posix+privileged stx*.self-apply1))))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~265ms
(define E.self-apply2 (with-time (lambda () (E-eval E.self-apply1))))
(displayln "evaluating self-apply2:")
;; ~5ms
(pretty-write (with-time (lambda () (E-eval E.self-apply2))))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
