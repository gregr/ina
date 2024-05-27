(define program (make-program))
(define (link-definition* env def*) (program-parse-definition* program env def*))
(define env.primitive            (package->env package.primitive))
(define env.primitive.privileged (env-conjoin*
                                   (package->env package.primitive.privileged)
                                   (package->env package.primitive.control.low-level.privileged)))
(define env.syntax               (package->env package.syntax))
(define env.privileged           (env-conjoin* env.minimal env.primitive env.primitive.privileged))
(define env.unprivileged         (env-conjoin* env.minimal env.primitive))
(define env.base                 (env-conjoin* env.unprivileged
                                               (link-definition* env.privileged def*.base)))
(define env.compiler             (link-definition* env.base def*.compiler))
(define env.nscheme              (let ((env.deps (env-conjoin* env.base env.syntax env.compiler)))
                                   (env-conjoin* env.deps env.meta
                                                 (link-definition* env.deps def*.nscheme))))
(define env.large                env.nscheme)
(define env.large.privileged     (env-conjoin* env.large env.primitive.privileged))
