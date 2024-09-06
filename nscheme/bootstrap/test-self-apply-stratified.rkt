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
  (include "../include/platform/bootstrap-stratified.scm")
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
  (append
   `((define def*.base     ',def*.base)
     (define def*.compiler ',def*.compiler)
     (define def*.nscheme  ',def*.nscheme)
     (define def*.text     ',def*.text))
   def*.primitive
   def*.syntax
   (file-name->stx* "../include/platform/bootstrap-stratified.scm")
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
(define E.self-apply1 (time (parse-body env.large+privileged stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.large+privileged stx*.self-apply1)))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~265ms
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
