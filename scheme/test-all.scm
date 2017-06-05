(printf "\nNaive evaluate\n")
(load "eval-naive.scm")
(load "test.scm")

(printf "\nHigher order evaluate\n")
(load "eval-ho.scm")
(load "test.scm")

(printf "\nFirst order evaluate\n")
(load "eval-fo.scm")
(load "test.scm")

(printf "\neval\n")
(define (evaluate expr env) (eval expr))
(load "test.scm")
