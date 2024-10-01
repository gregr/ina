(define env.common           (value-package->env package.common))
(define env.control          (value-package->env package.control))
(define env.privileged       (env-conjoin* (value-package->env package.privileged)
                                           ;; TODO: eliminate the need for this during bootstrap
                                           (value-package->env package.low-level:control)))
(define env.syntax           (value-package->env package.syntax))
(define env.tiny             (env-conjoin* env.minimal env.common))
(define env.small            (env-conjoin* env.tiny env.control))
(define env.small+privileged (env-conjoin* env.small env.privileged))
(define env.base             (env-conjoin* env.small
                                           (eval-definition* env.small+privileged def*.base)))
(define env.compiler         (eval-definition* env.base def*.compiler))
(define env.nscheme          (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                               (env-conjoin* env.deps env.meta
                                             (eval-definition* env.deps def*.nscheme))))
(define env.text             (eval-definition* env.base def*.text))
(define env.large            (env-conjoin* env.nscheme env.text))
(define env.large+privileged (env-conjoin* env.large env.privileged))

(define env.posix.common (value-package->env package.posix.common))
(define env.posix        (env-conjoin* (eval-definition* (env-conjoin* env.base env.posix.common)
                                                         def*.posix)
                                       env.posix.common))
(define env.large+posix+privileged (env-conjoin* env.large+privileged env.posix))
