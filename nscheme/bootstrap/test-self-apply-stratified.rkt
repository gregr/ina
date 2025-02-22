#lang racket/base
(require racket/include "../platform/racket/nscheme.rkt" "include.rkt")
(nscheme-run (include "../test/eval-self-apply-stratified.scm"))
