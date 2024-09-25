(define program (make-program))
(define (link-definition* env def*) (program-parse-definition* program env def*))
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
                                           (link-definition* env.small+privileged def*.base)))
(define env.compiler         (link-definition* env.base def*.compiler))
(define env.nscheme          (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                               (env-conjoin* env.deps env.meta
                                             (link-definition* env.deps def*.nscheme))))
(define env.text             (link-definition* env.base def*.text))
(define env.large            (env-conjoin* env.nscheme env.text))
(define env.large+privileged (env-conjoin* env.large env.privileged))

(define env.posix.privileged       (value-package->env package.posix.privileged))
(define env.large+posix+privileged (env-conjoin* env.large+privileged env.posix.privileged))
