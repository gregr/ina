#lang racket/base
(provide (all-defined-out))
(require
  ;; Toggle commenting on these two to turn on interrupt-aware lambdas.
  "../platform/racket/nscheme.rkt"
  ;(rename-in "../platform/racket/nscheme.rkt" (interruptible-lambda lambda))
  racket/file racket/include racket/pretty racket/runtime-path racket/splicing)

(include "../include/base/misc.scm")
(include "../include/boot/record.scm")
(include "../include/boot/string.scm")
(include "../include/boot/control.scm")
(include "../include/base/compare.scm")
(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/string.scm")
(include "../include/base/exception.scm")
(include "../include/base/restart.scm")
(include "../include/base/raise.scm")

(include "../include/syntax.scm")
(include "../include/ir.scm")
(include "../include/stage-simple.scm")
(include "../include/parse.scm")
(include "../include/primitive.scm")
(include "../include/minimal.scm")
(include "../include/match.scm")

;; TODO: all remaining compiler definitions should be included here, replacing eval-simple.scm:
(include "../include/eval-simple.scm")

;; TODO: the parsers defined here perform compile-time evaluation.  They should be adjusted to
;; depend on the compiler instead of E-eval:
(include "../include/extended.scm")

(define-runtime-path path.here ".")

(define (file-name->stx* fname)
  (call-with-input-file
    (build-path path.here fname)
    (lambda (in)
      (let loop ()
        (let ((x (read in)))
          (cond ((eof-object? x) '())
                (else            (cons x (loop)))))))))

(define (file-name*->stx* fname*) (apply append (map file-name->stx* fname*)))

(define def*.include/boot
  (file-name*->stx*
    '("../include/base/misc.scm"  ; convenient for bootstrap, but no privilege needed
      "../include/boot/record.scm"
      "../include/boot/string.scm"
      "../include/boot/control.scm"
      )))
(define def*.include/base
  (file-name*->stx*
    '("../include/base/compare.scm"
      "../include/base/pair.scm"
      "../include/base/list.scm"
      "../include/base/number.scm"
      "../include/base/mvector.scm"
      "../include/base/vector.scm"
      "../include/base/mbytevector.scm"
      "../include/base/bytevector.scm"
      "../include/base/string.scm"
      "../include/base/exception.scm"
      "../include/base/restart.scm"
      "../include/base/raise.scm"
      )))
(define def*.include
  (file-name*->stx*
    '("../include/syntax.scm"
      "../include/ir.scm"
      "../include/stage-simple.scm"
      "../include/parse.scm"
      "../include/minimal.scm"
      "../include/match.scm")))
(define def*.primitive (file-name->stx* "../include/primitive.scm"))
(define def*.eval      (file-name->stx* "../include/eval-simple.scm"))
(define def*.extended  (file-name->stx* "../include/extended.scm"))

(define env.primitive.privileged.all
  (env-compose env.primitive.privileged env.primitive.privileged.control))
(define env.include/boot
  (env-compose* (eval-definition*
                  (env-compose* env.primitive.privileged.all env.primitive env.minimal)
                  def*.include/boot)
                env.primitive
                env.minimal))
(define env.include/base
  (env-compose env.include/boot (eval-definition* env.include/boot def*.include/base)))
(define env.include.0
  (env-compose env.include/base (eval-definition* env.include/base def*.include)))
(define env.include
  (env-compose env.include.0
               (eval-definition* (env-compose env.primitive.privileged.all env.include.0)
                                 def*.primitive)))
(define env.eval
  (eval-definition* (env-compose* env.primitive.privileged.all env.include)
                    def*.eval))
(define env.include.extended
    (env-compose* (eval-definition* (env-compose* env.eval env.include)
                                    def*.extended)
                  env.include))

;; Old notes:

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
