(define env.primitive.privileged.all.0
  (env-conjoin env.primitive.privileged.control env.primitive.privileged))
(define env.privileged.0
  (env-conjoin* env.minimal env.primitive env.primitive.privileged.all.0))
(define env.unprivileged.0
  (env-conjoin env.minimal env.primitive))
(define env.include/base/early
  (eval-definition* env.privileged.0 def*.include/base/early))
(define env.include/base/early.0
  (env-conjoin env.privileged.0 env.include/base/early))
(define env.include/boot
  (eval-definition* env.include/base/early.0 def*.include/boot))
(define env.include/boot.0
  (env-conjoin env.include/base/early.0 env.include/boot))
(define env.include/base
  (env-conjoin env.include/base/early (eval-definition* env.include/boot.0 def*.include/base)))
(define env.include/base.0
  (env-conjoin env.unprivileged.0 env.include/base))
(define env.include
  (env-conjoin env.include/base (eval-definition* env.include/base.0 def*.include)))
(define env.include.0
  (env-conjoin env.unprivileged.0 env.include))
(define env.eval
  (eval-definition* env.include.0 def*.eval))
(define env.primitive-environments
  (eval-definition* (env-conjoin env.include.0 env.primitive.privileged.all.0)
                    def*.primitive-environments))
(define env.minimal.1
  (E-eval (parse-expression env.include 'env.minimal)))
(define env.primitive.1
  (E-eval (parse-expression env.primitive-environments 'env.primitive)))
(define env.primitive.privileged.1
  (E-eval (parse-expression env.primitive-environments 'env.primitive.privileged)))
(define env.primitive.privileged.control.1
  (E-eval (parse-expression env.primitive-environments 'env.primitive.privileged.control)))
(define env.primitive.privileged.all.1
  (env-conjoin env.primitive.privileged.control.1 env.primitive.privileged.1))
(define env.unprivileged.1
  (env-conjoin env.minimal.1 env.primitive.1))
(define env.include/base.1
  (env-conjoin env.unprivileged.1 env.include/base))
(define env.include.1
  (env-conjoin env.unprivileged.1 env.include))
(define env.extended.1
  (E-eval (parse-expression env.eval 'env.extended)))
(define env.large
  (env-conjoin* env.extended.1 env.eval env.include.1 env.primitive-environments))
