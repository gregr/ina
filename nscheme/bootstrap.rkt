#lang racket/base
(require "nscheme.rkt" racket/include racket/match racket/splicing racket/vector
         (for-syntax racket/base))
(include "include/base-1-source.scm")
(include "include/ast.scm")
(include "include/syntax.scm")

(define-syntax (quote-syntax stx)
  (syntax-case stx ()
    ((_ stx)
     (let loop ((stx #'stx))
       (define (remap x)
         (cond ((pair?   x) #`(cons #,(loop (car x)) #,(loop (cdr x))))
               ((vector? x) #`(vector . #,(map loop (vector->list x))))
               (else        #`'#,x)))
       (if (syntax? stx)
           #`(syntax-provenance-set #,(remap (syntax-e stx))
                                    '#,(list (cons 'source   (syntax-source   stx))
                                             (cons 'position (syntax-position stx))
                                             (cons 'span     (syntax-span     stx))
                                             (cons 'line     (syntax-line     stx))
                                             (cons 'column   (syntax-column   stx))))
           (remap stx))))))

(define-syntax (quasiquote-syntax stx)
  (syntax-case stx ()
    ((_ qq.0)
     (let loop ((qq #'qq.0) (level 0))
       (define (stx->pv stx)
         (list (cons 'source   (syntax-source   stx))
               (cons 'position (syntax-position stx))
               (cons 'span     (syntax-span     stx))
               (cons 'line     (syntax-line     stx))
               (cons 'column   (syntax-column   stx))))
       (syntax-case qq (quasiquote-syntax unsyntax unsyntax-splicing)
         ((quasiquote-syntax q)        #`(syntax-provenance-set
                                           (list (quote-syntax #,#'quasiquote-syntax)
                                                 #,(loop #'q (+ level 1)))
                                           '#,(stx->pv qq)))
         ((unsyntax e)                 (if (= level 0)
                                           #'e
                                           #`(syntax-provenance-set
                                               (list (quote-syntax #,#'unsyntax)
                                                     #,(loop #'e (- level 1)))
                                               '#,(stx->pv qq))))
         (((unsyntax-splicing e) . qd) (if (= level 0)
                                           #`(append (syntax->list e) #,(loop #'qd level))
                                           #`(syntax-provenance-set
                                               (cons (list (quote-syntax #,#'unsyntax-splicing)
                                                           #,(loop #'e (- level 1)))
                                                     #,(loop #'qd level))
                                               '#,(stx->pv qq))))
         ((quasiquote-syntax . _)      (error "invalid use of keyword" qq))
         ((unsyntax          . _)      (error "invalid use of keyword" qq))
         ((unsyntax-splicing . _)      (error "invalid use of keyword" qq))
         ((qq.a . qq.d)                #`(syntax-provenance-set
                                           (cons #,(loop #'qq.a level) #,(loop #'qq.d level))
                                           '#,(stx->pv qq)))
         (quasiquote-syntax            (error "invalid use of keyword" qq))
         (unsyntax                     (error "invalid use of keyword" qq))
         (unsyntax-splicing            (error "invalid use of keyword" qq))
         (_                            (if (vector? (syntax-e qq))
                                           #`(syntax-provenance-set
                                               (vector . #,(map (lambda (qq) (loop qq level))
                                                                (vector->list (syntax-e qq))))
                                               '#,(stx->pv qq))
                                           #`(quote-syntax #,qq))))))))

(include "include/parse.scm")
(include "include/boot/env-primitive.scm")
(include "include/base-0-parse.scm")
(include "include/ast-eval.scm")
;; TODO: read, parse, and ast-eval "include/base-1-source.scm"
;; TODO: pass ast.base-1 into:
;(include "include/base-2-parse.scm")
;; TODO: read, parse, and ast-eval "include/syntax.scm"
;; TODO: pass ast.syntax into:
;(include "include/base-3-parse.scm")

;; TODO: bootstrap sequence:
;; - bootstrap syntax.scm
;; - bootstrap ast.scm
;; - bootstrap parse.scm
;; - bootstrap base-0-parse.scm
;;   - will depend on Racket-simulated quote-syntax quasiquote-syntax syntax-dismantle
;;   - produce all operators needed by base-1-source.scm (operators that don't embed procedure references)
;; - bootstrap ast-eval.scm
;;   - can be used to produce "final" versions
;;   - should be metacircular for target-agnostic bootstrapping
;;     - use a privileged primitive to assign new procedure metadata
;; - final base-1-source.scm
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
