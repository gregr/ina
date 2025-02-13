#lang racket/base
(require racket/include "../platform/racket/nscheme.rkt" "include.rkt")
(nscheme-run
 (include "../include/platform/env/evaluated.scm")
 (include "../include/platform/posix/env/evaluated.scm")
 (include "../test/eval-self-apply-mixed.scm"))
