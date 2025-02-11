#lang racket/base
(require "../platform/racket/nscheme.rkt" "include.rkt")
(nscheme-run
 "../include/platform/env/primitive.scm"
 "../test/mk.scm")
