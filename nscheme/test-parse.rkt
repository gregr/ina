#lang racket/base
(require "nscheme.rkt" racket/include racket/match racket/splicing racket/vector)
(include "include/base.scm")
(include "include/syntax.scm")
(include "include/ast.scm")
(include "include/ast-eval.scm")
(include "include/parse.scm")

;; TODO: bootstrap sequence:
;; - bootstrap syntax.scm
;; - bootstrap ast.scm
;; - bootstrap parse.scm
;; - bootstrap base-1-parse.scm
;;   - will depend on Racket-simulated quote-syntax quasiquote-syntax syntax-dismantle
;;   - produce all operators needed by base-0-source.scm (operators that don't embed procedure references)
;; - bootstrap ast-eval.scm
;;   - can be used to produce "final" versions
;;   - should be metacircular for target-agnostic bootstrapping
;;     - use a privileged primitive to assign new procedure metadata
;; - final base-0-source.scm
;;   - not equal? append memp member errors etc.
;; - bootstrap base-2-parse.scm
;;   - case quasiquote etc.
;; - final syntax.scm
;; - bootstrap base-3-parse.scm
;;   - quasiquote-syntax syntax-dismantle
;;   - since we define parsers at the meta level, these too can be defined in terms of their Racket-simulated selves
;; - final ast.scm
;; - final parse.scm
;; - final base-1-parse.scm
;; - final base-2-parse.scm
;; - final base-3-parse.scm
;; - bootstrap base-4-parse.scm
;;   - this can't be final because it depends on ast-eval
;;   - begin-meta declare-parser define-syntax etc.
;; - final base-5-source.scm
;;   - no need for a bootstrap version
;; - final ast-eval.scm
;;   - if this doesn't have dependencies on base-5-source.scm, we could move this before base-4-parse.scm to not
;;     have to bootstrap that separately
;; - final base-4-parse.scm
;; - final interact.scm
;;   - maybe better named as bootstrap-the-interactive-repl.scm
;;   - this contains a single expression (for a thunk), not a list of definitions
;;     - each base-N-parse.scm file produces an env transformer that needs to be invoked
;;     - this thunk will run all transformers sequentially, then use the result env to start a REPL
;; - bootstrap compiler-with-backend-for-our-target-platform.scm
;; - snapshot/compile the final interact.scm thunk to our target platform
;;
;; NOTE: none of the "final" artifacts have to be "run" during Racket evaluation.  They are only used as carriers of
;; their definition metadata, which is accessed to build a snapshot.  The first time they will be "run", they will be
;; executed on the target platform, after the cross-compilation step.  So ast-eval calling conventions don't really
;; matter, except for convenient debugging.
;; - Unless final base-5-source.scm or final ast-eval.scm make use of begin-meta or declare-parser (or define-syntax).
;;   If they do, we can always defer their parsing, pushing it into the interaction thunk, so that they aren't parsed
;;   until they've been compiled to the target platform, using the target's ast-eval during that parsing.  Except this
;;   won't work if parsing interact.scm also depends on meta-level evaluation, right?  Maybe we can avoid that.
;;   - We can avoid the problem by moving ast-eval.scm and interact.scm before base-4-parse.scm, have interact.scm
;;     embed the unparsed code for base-4-parse.scm and base-5-source.scm, and have it be responsible for parsing them
;;     as part of the env-building pipeline.  It should be able to parse them using the base-3 environment it builds.
;;
;; TODO: include read.scm, write.scm, etc. for complete independence
