;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

;;; This variant of bootstrapping is unstratified, mixing values computed at different phases.  That
;;; means compilation must support cross-stage persistence.

(define (with-time thunk) (with-milliseconds displayln thunk))

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
  ;; ~0ms
  (define E.test (with-time (lambda () (parse-body env.large stx*.test))))
  (displayln "evaluating test:")
  ;; ~0ms
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
   (file-name->stx* "../include/platform/env/evaluated.scm")
   '((define stx*.test (list '(list
                               (+ 1 2)
                               (foldr + 0 '(1 2 3 4 5))
                               (foldr cons '(1 2) '(3 4 5))
                               (foldl cons '(1 2) '(3 4 5))
                               (fold-left (lambda (acc x y) (cons (cons x y) acc))
                                          '(1 2) '(3 4 5) '(a b c))
                               (fold-right (lambda (acc x y) (cons (cons x y) acc))
                                           '(1 2) '(3 4 5) '(a b c)))))
     (parse-body env.large stx*.test))))


(displayln "parsing self-apply1:")
;; ~2ms
(define E.self-apply1 (with-time (lambda () (parse-body env.large+posix+privileged stx*.self-apply1))))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~553ms
(define E.self-apply2 (with-time (lambda () (E-eval E.self-apply1))))
(displayln "evaluating self-apply2:")
;; ~1ms
(pretty-write (with-time (lambda () (E-eval E.self-apply2))))
;==>
;(3
; 15
; (3 4 5 1 2)
; (5 4 3 1 2)
; ((5 . c) (4 . b) (3 . a) 1 2)
; ((3 . a) (4 . b) (5 . c) 1 2))
