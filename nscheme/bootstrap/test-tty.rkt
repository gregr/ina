#lang racket/base
(require
  "../platform/racket/nscheme.rkt" "include.rkt"
  racket/include (prefix-in rkt: racket/base) (prefix-in rkt: racket/pretty))
(print-as-expression #f)
(rkt:pretty-print-abbreviate-read-macros #f)
(include "../test/tty.scm")
