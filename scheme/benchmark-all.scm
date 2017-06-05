(printf "\nNaive evaluate\n")
(load "eval-naive.scm")
(load "benchmark.scm")

(printf "\nHigher order evaluate\n")
(load "eval-ho.scm")
(load "benchmark.scm")

(printf "\nFirst order evaluate\n")
(load "eval-fo.scm")
(load "benchmark.scm")

(printf "\neval\n")
(define (evaluate expr env) (eval expr))
(load "benchmark.scm")
