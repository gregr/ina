#lang racket/base

(displayln (current-directory))
(displayln (find-system-path 'orig-dir))
(displayln (find-system-path 'run-file))
(displayln (find-system-path 'exec-file))
(displayln (current-command-line-arguments))
