(define program (make-program))
(define (link-definition* env def*) (program-parse-definition* program env def*))
(define env.primitive            (value-package->env package.primitive))
(define env.primitive.privileged (env-conjoin*
                                   (value-package->env package.primitive.privileged)
                                   (value-package->env package.primitive.control.low-level.privileged)))
(define env.syntax               (value-package->env package.syntax))
(define env.privileged           (env-conjoin* env.minimal env.primitive env.primitive.privileged))
(define env.unprivileged         (env-conjoin* env.minimal env.primitive))
(define env.base                 (env-conjoin* env.unprivileged
                                               (link-definition* env.privileged def*.base)))
(define env.compiler             (link-definition* env.base def*.compiler))
(define env.nscheme              (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                                   (env-conjoin* env.deps env.meta
                                                 (link-definition* env.deps def*.nscheme))))
(define env.text                 (link-definition* env.base def*.text))
(define env.large                (env-conjoin* env.nscheme env.text))
(define env.large.privileged     (env-conjoin* env.large env.primitive.privileged))
