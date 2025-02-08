(define env.base             (env-conjoin* env.small
                                           (eval-definition* env.small+privileged def*.base)))
(define env.compiler         (eval-definition* env.base def*.compiler))
(define env.nscheme          (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                               (env-conjoin* env.deps env.meta
                                             (eval-definition* env.deps def*.nscheme))))
(define env.large            env.nscheme)
(define env.large+privileged (env-conjoin* env.large env.privileged))
