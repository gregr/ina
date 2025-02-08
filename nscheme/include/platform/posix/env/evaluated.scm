(define env.posix
  (env-conjoin* (eval-definition* (env-conjoin* env.base env.posix.common) def*.posix)
                env.posix.common))
(define env.large+posix+privileged (env-conjoin* env.large+privileged env.posix))
