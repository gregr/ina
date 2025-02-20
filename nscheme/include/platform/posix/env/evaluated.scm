(define env.posix       (eval-definition* env.base def*.posix))
(define env.large+posix (env-conjoin* env.large env.posix))
