(define program (make-program))
(define (link-definition* env def*) (program-link-definition* program env def*))
(define env.primitive.privileged.all.0
  (env-conjoin env.primitive.privileged.control env.primitive.privileged))
(define env.privileged.0
  (env-conjoin* env.minimal env.primitive env.primitive.privileged.all.0))
(define env.unprivileged.0
  (env-conjoin env.minimal env.primitive))
(define env.include/base/early
  (link-definition* env.privileged.0 def*.include/base/early))
(define env.include/base/early.0
  (env-conjoin env.privileged.0 env.include/base/early))
(define env.include/boot
  (link-definition* env.include/base/early.0 def*.include/boot))
(define env.include/boot.0
  (env-conjoin env.include/base/early.0 env.include/boot))
(define env.include/base
  (env-conjoin env.include/base/early (link-definition* env.include/boot.0 def*.include/base)))
(define env.include/base.0
  (env-conjoin env.unprivileged.0 env.include/base))
(define env.include
  (env-conjoin env.include/base (link-definition* env.include/base.0 def*.include)))
(define env.include.0
  (env-conjoin env.unprivileged.0 env.include))
(define env.eval
  (link-definition* env.include.0 def*.eval))
(define env.test
  (env-conjoin env.include.0 env.eval))
