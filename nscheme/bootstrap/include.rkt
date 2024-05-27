#lang racket/base
(provide (all-defined-out))
(require
  ;; Toggle commenting on these two to turn on interrupt-aware lambdas.
  "../platform/racket/nscheme.rkt"
  ;(rename-in "../platform/racket/nscheme.rkt" (interruptible-lambda lambda))
  racket/file racket/include racket/local racket/pretty racket/runtime-path racket/splicing)

(include "../include/base/misc.scm")
(include "../include/base/control.scm")
(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/string.scm")
(include "../include/base/record.scm")
(include "../include/base/exception.scm")
(include "../include/base/restart.scm")
(include "../include/base/raise.scm")

(include "../include/syntax.scm")

(include "../include/compiler/high-level-ir.scm")
(include "../include/compiler/backend/rkt.scm")

(include "../include/nscheme/stage.scm")
(include "../include/nscheme/parse.scm")
(include "../include/nscheme/minimal.scm")
(include "../include/nscheme/match.scm")
;; TODO: these definitions perform compile-time evaluation.  They should be adjusted to
;; depend on the compiler instead of E-eval:
(include "../include/nscheme/program.scm")
(include "../include/nscheme/meta.scm")

(include "../include/platform/primitive.scm")
(include "../include/platform/primitive-privileged.scm")
(include "../include/platform/primitive-control-low-level-privileged.scm")

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

(define def*.base
  (file-name*->stx*
   '("../include/base/misc.scm"
     "../include/base/control.scm"
     "../include/base/pair.scm"
     "../include/base/list.scm"
     "../include/base/number.scm"
     "../include/base/mvector.scm"
     "../include/base/vector.scm"
     "../include/base/mbytevector.scm"
     "../include/base/bytevector.scm"
     "../include/base/string.scm"
     "../include/base/record.scm"
     "../include/base/exception.scm"
     "../include/base/restart.scm"
     "../include/base/raise.scm"
     )))
(define def*.syntax
  (file-name*->stx*
   '("../include/syntax.scm")))
(define def*.compiler
  (file-name*->stx*
   '("../include/compiler/high-level-ir.scm"
     "../include/compiler/backend/rkt.scm"
     )))
(define def*.nscheme
  (file-name*->stx*
   '("../include/nscheme/stage.scm"
     "../include/nscheme/parse.scm"
     "../include/nscheme/minimal.scm"
     "../include/nscheme/match.scm"
     "../include/nscheme/program.scm"
     "../include/nscheme/meta.scm"
     )))
(define def*.primitive
  (file-name*->stx*
   '("../include/platform/primitive.scm"
     "../include/platform/primitive-privileged.scm"
     "../include/platform/primitive-control-low-level-privileged.scm"
     )))

(include "../include/platform/bootstrap.scm")

(define (ns-eval E)
  (with-pretty-panic (with-native-signal-handling (lambda () (E-eval E)))))


;; Old notes:

;; TODO: compile interact.scm and all of its dependencies (almost everything listed above).
;; - Other dependencies: read.scm write.scm terminal-control.scm and/or other UI definitions, etc.
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
