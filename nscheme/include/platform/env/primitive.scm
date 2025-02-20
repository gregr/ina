(define env.common  (value-package->env package.common))
(define env.control (value-package->env package.control))
(define env.tiny    (env-conjoin* env.minimal env.common))
(define env.small   (env-conjoin* env.tiny env.control))
