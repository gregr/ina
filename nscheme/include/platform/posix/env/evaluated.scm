(define env.posix                  (eval-definition* env.base def*.posix))
(define env.large+posix+privileged (env-conjoin* env.large+privileged env.posix))
