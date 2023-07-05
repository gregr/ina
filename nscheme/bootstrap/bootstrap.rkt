#lang racket/base
(require "../platform/racket/nscheme.rkt"
         racket/file racket/include racket/pretty racket/runtime-path racket/splicing)

;;; This program runs a minimal, ahead-of-time cross-compilation process on the code for an
;;; interactive system and its dependencies, targeting each platform.

(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/boot/record.scm")
(include "../include/boot/error.scm")
(include "../include/base/misc.scm")
(include "../include/base/compare.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/string.scm")

(include "../include/syntax.scm")
(include "../include/ir.scm")
(include "../include/stage-simple.scm")
(include "../include/parse.scm")
(include "../include/primitive.scm")
(include "../include/minimal.scm")
(include "../include/match.scm")

(define-runtime-path path.here ".")

(define stx*
  (apply append
         (map (lambda (fname)
                (call-with-input-file
                  (build-path path.here fname)
                  (lambda (in)
                    (let loop ()
                      (let ((x (read in)))
                        (cond ((eof-object? x) '())
                              (else            (cons x (loop)))))))))
              '("../include/base/pair.scm"
                "../include/base/list.scm"
                "../include/base/number.scm"
                "../include/boot/record.scm"
                "../include/boot/error.scm"
                "../include/base/misc.scm"
                "../include/base/compare.scm"
                "../include/base/mvector.scm"
                "../include/base/vector.scm"
                "../include/base/mbytevector.scm"
                "../include/base/bytevector.scm"
                "../include/base/string.scm"
                "../include/syntax.scm"
                "../include/ir.scm"
                "../include/stage-simple.scm"
                "../include/parse.scm"
                "../include/primitive.scm"
                "../include/minimal.scm"
                "../include/match.scm"))))

(require profile)

(define env.test (env-compose.match (env-compose env.primitive.privileged
                                                 (env-compose env.primitive env.minimal))))
(pretty-write (env-describe env.test))
(define stx*.test (append stx*
                          (list
                            ;'(+ 1 2)
                            ;'(foldr + 0 '(1 2 3 4 5))
                            ;'(foldr cons '(1 2) '(3 4 5))
                            ;'(foldl cons '(1 2) '(3 4 5))
                            ;'(foldl (lambda (x y acc) (cons (cons x y) acc))
                            ;        '(1 2) '(3 4 5) '(a b c))
                            '(foldr (lambda (x y acc) (cons (cons x y) acc))
                                    '(1 2) '(3 4 5) '(a b c))
                            )))
(displayln "parsing test:")
;; ~47ms
(define E.test (time (parse-body env.test stx*.test)))
;(define E.test (profile (parse-body env.test stx*.test)))
;(pretty-write E.test)
;(pretty-write (E-pretty E.test))
(displayln "evaluating test:")
;; ~4ms
(pretty-write (time (E-eval E.test)))
;==> ((3 . a) (4 . b) (5 . c) 1 2)

(define stx*.self-apply1
  (append stx*
          (list
            `(let ((env.test
                     (env-compose.match (env-compose env.primitive.privileged
                                                     (env-compose env.primitive env.minimal))))
                   (stx*.test (append ',stx*
                                      (list
                                        '(foldr (lambda (x y acc) (cons (cons x y) acc))
                                                '(1 2) '(3 4 5) '(a b c))))))
               (parse-body env.test stx*.test)))))
(displayln "parsing self-apply1:")
;; ~49ms
(define E.self-apply1 (time (parse-body env.test stx*.self-apply1)))
;(define E.self-apply1 (profile (parse-body env.test stx*.self-apply1)))
;(pretty-write E.self-apply1)
;(pretty-write (E-pretty E.self-apply1))
(displayln "evaluating self-apply1 to parse self-apply2:")
;; ~7573ms
(define E.self-apply2 (time (E-eval E.self-apply1)))
;(define E.self-apply2 (profile (E-eval E.self-apply1)))
(displayln "evaluating self-apply2:")
;; ~4ms
(pretty-write (time (E-eval E.self-apply2)))
;==> ((3 . a) (4 . b) (5 . c) 1 2)

;; TODO: all remaining compiler definitions should be included here, replacing eval.rkt:
(require "eval.rkt")

;; TODO: the parsers defined here perform compile-time evaluation.  They should be adjusted to
;; depend on the compiler instead of E-eval:
(include "../include/extended.scm")

;; TODO: compile interact.scm and all of its dependencies (almost everything listed above).
;; - Other dependencies: read.scm write.scm tty.scm and/or other UI definitions, etc.
;; - Perform this comilation for all desired target platforms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some idea sketches:
;;
;; Examples of possible io procedure operation names:
;; - 'block-read 'block-write 'stream-read 'stream-write 'socket-read 'socket-write
;; - What about handles to virtual filesystems and networks?  These aren't blocks or streams.
;;   - Ultimately, we can't predetermine all primitive families, and platforms will differ in the
;;     set of primitives they support.
;;     - e.g., a gui-enabled platform vs. a text-only platform
;; - IO descriptors could just be integer ids, or we could tag them with a type, e.g.:
;;   - `(file ,uid)
;;   - `(console ,uid)
;;   - `(process ,uid)
;;   - `(keyboard ,uid)
;;   - `(mouse ,uid)
;;   - `(canvas ,uid)
;;   - `(audio ,uid)
;;   - `(filesystem ,uid)
;;   - `(network ,uid)
;; - The host system would map each descriptor to a device, which has its own state and controller.
;;   - When taking a full-system snapshot, we may want to package up the state of some devices.
;;   - file/canvas/console/etc.
;;
;; We will need to deal with dynamic linking, and conflicting layouts of shared libraries.  We also
;; may not be restarting a system.  We may just be recovering persisted data and using it from an
;; existing system.  We also may be restarting a system while embedding it in another system.
;; - Components of an independent executable:
;;   - bootstrapping process
;;   - shared libraries
;;   - program entry point
;;   - optional io device state
;; - These components can take different forms even on the same platform:
;;   - bootstrapping process
;;     - wrapper for standalone executable file format
;;     - virtual machine executable taking libraries and program as input
;;     - loading additional libraries into an already-running program
;;   - shared libraries, program, io device state
;;     - raw memory dump (a heap image)
;;     - generated code
;;       - platform-specific (e.g., Racket, Chez, JS, Python, C, WASM, x86 ...), compiled code
;;       - portable, compiled code
;;       - portable, not-yet-compiled nScheme code

;; TODO: do we need quote-syntax and quasiquote-syntax for bootstrapping?
;(define-syntax (quote-syntax stx)
;  (syntax-case stx ()
;    ((_ stx)
;     (let loop ((stx #'stx))
;       (define (remap x)
;         (cond ((pair?   x) #`(cons #,(loop (car x)) #,(loop (cdr x))))
;               ((vector? x) #`(vector . #,(map loop (vector->list x))))
;               (else        #`'#,x)))
;       (if (syntax? stx)
;           #`(syntax-provenance-set #,(remap (syntax-e stx))
;                                    '#,(list (cons 'source   (syntax-source   stx))
;                                             (cons 'position (syntax-position stx))
;                                             (cons 'span     (syntax-span     stx))
;                                             (cons 'line     (syntax-line     stx))
;                                             (cons 'column   (syntax-column   stx))))
;           (remap stx))))))
;
;(define-syntax (quasiquote-syntax stx)
;  (syntax-case stx ()
;    ((_ qq.0)
;     (let loop ((qq #'qq.0) (level 0))
;       (define (stx->pv stx)
;         (list (cons 'source   (syntax-source   stx))
;               (cons 'position (syntax-position stx))
;               (cons 'span     (syntax-span     stx))
;               (cons 'line     (syntax-line     stx))
;               (cons 'column   (syntax-column   stx))))
;       (syntax-case qq (quasiquote-syntax unsyntax unsyntax-splicing)
;         ((quasiquote-syntax q)        #`(syntax-provenance-set
;                                           (list (quote-syntax #,#'quasiquote-syntax)
;                                                 #,(loop #'q (+ level 1)))
;                                           '#,(stx->pv qq)))
;         ((unsyntax e)                 (if (= level 0)
;                                           #'e
;                                           #`(syntax-provenance-set
;                                               (list (quote-syntax #,#'unsyntax)
;                                                     #,(loop #'e (- level 1)))
;                                               '#,(stx->pv qq))))
;         (((unsyntax-splicing e) . qd) (if (= level 0)
;                                           #`(append (syntax->list e) #,(loop #'qd level))
;                                           #`(syntax-provenance-set
;                                               (cons (list (quote-syntax #,#'unsyntax-splicing)
;                                                           #,(loop #'e (- level 1)))
;                                                     #,(loop #'qd level))
;                                               '#,(stx->pv qq))))
;         ((quasiquote-syntax . _)      (error "invalid use of keyword" qq))
;         ((unsyntax          . _)      (error "invalid use of keyword" qq))
;         ((unsyntax-splicing . _)      (error "invalid use of keyword" qq))
;         ((qq.a . qq.d)                #`(syntax-provenance-set
;                                           (cons #,(loop #'qq.a level) #,(loop #'qq.d level))
;                                           '#,(stx->pv qq)))
;         (quasiquote-syntax            (error "invalid use of keyword" qq))
;         (unsyntax                     (error "invalid use of keyword" qq))
;         (unsyntax-splicing            (error "invalid use of keyword" qq))
;         (_                            (if (vector? (syntax-e qq))
;                                           #`(syntax-provenance-set
;                                               (vector . #,(map (lambda (qq) (loop qq level))
;                                                                (vector->list (syntax-e qq))))
;                                               '#,(stx->pv qq))
;                                           #`(quote-syntax #,qq))))))))
;
;(require racket/pretty)
;(pretty-write (quote-syntax (foo . bar)))
;(pretty-write (quasiquote-syntax (a b c)))
;(pretty-write (quasiquote-syntax (a #,'b c)))
;(pretty-write (quasiquote-syntax (a #,'(1 2 3) c)))
;(pretty-write (quasiquote-syntax (a #,@(quote-syntax (1 2 3)) c)))
;(pretty-write (quasiquote-syntax (a (quasiquote-syntax (1 #,2 3)) c)))
