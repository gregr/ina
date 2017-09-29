(printf "\nNaive evaluate\n")
(load "eval-naive.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\nHigher order evaluate\n")
(load "eval-ho.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\nFirst order evaluate\n")
(load "eval-fo.scm")
(load "test-defs.scm")
(load "test.scm")

(printf "\neval\n")
(define (vector-reify v)
  (if (vector? v)
    v
    (error 'vector-reify (format "invalid vector ~s" v))))
(define (evaluate expr env) (eval expr))
(load "test-defs.scm")
(load "test.scm")
