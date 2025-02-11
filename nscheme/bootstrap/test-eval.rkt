#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt")
(nscheme-run
 "../include/platform/env/primitive.scm"
 "../include/platform/posix/env/include.scm"
 "../include/platform/env/evaluated.scm"
 "../include/platform/posix/env/primitive.scm"
 "../include/platform/posix/env/evaluated.scm"
 "../test/eval.scm")
