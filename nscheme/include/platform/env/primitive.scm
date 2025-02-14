(define env.common           (value-package->env package.common))
(define env.control          (value-package->env package.control))
(define env.io               (value-package->env package.io))
(define env.privileged       (value-package->env package.privileged))
(define env.tiny             (env-conjoin* env.minimal env.common))
(define env.small            (env-conjoin* env.tiny env.control env.io))
(define env.small+privileged (env-conjoin* env.small env.privileged))
