(define program (make-program))
(define (link-definition* env def*) (program-link-definition* program env def*))
(define env.primitive.privileged (package->env package.primitive.privileged))
(define env.primitive            (package->env package.primitive))
(define env.syntax               (package->env package.syntax))
(define env.privileged           (env-conjoin* env.minimal env.primitive env.primitive.privileged))
(define env.unprivileged         (env-conjoin env.minimal env.primitive))
(define env.include/base/early   (link-definition* env.privileged def*.include/base/early))
(define env.include/base/early.privileged
  (env-conjoin env.privileged env.include/base/early))
(define env.include/boot
  (env-conjoin env.include/base/early.privileged
               (link-definition* env.include/base/early.privileged def*.include/boot)))
(define env.include/base
  (env-conjoin* env.unprivileged env.include/base/early
                (link-definition* env.include/boot def*.include/base)))
(define env.include
  (env-conjoin* env.include/base env.syntax
                (link-definition* (env-conjoin env.include/base env.syntax) def*.include)))
(define env.eval             (link-definition* env.include def*.eval))
(define env.large            (env-conjoin* env.include env.eval env.extended))
(define env.large.privileged (env-conjoin env.large env.primitive.privileged))
