#lang racket/base
(provide nscheme-boot nscheme-run)
(require
  ;; Toggle commenting on these two to turn on interrupt-aware lambdas.
  "../platform/racket/nscheme.rkt"
  ;(rename-in "../platform/racket/nscheme.rkt" (interruptible-lambda lambda))
  racket/include racket/local racket/splicing (prefix-in rkt: racket/base))

(define-syntax-rule (nscheme-boot path ...)
  (with-native-signal-handling (lambda () (include path) ...)))

(define-syntax-rule (nscheme-run path ...)
  (nscheme-boot
   "../include/base/misc.scm"
   "../include/base/pair.scm"
   "../include/base/list.scm"
   "../include/base/number.scm"
   "../include/base/mvector.scm"
   "../include/base/vector.scm"
   "../include/base/mbytevector.scm"
   "../include/base/bytevector.scm"
   "../include/base/unicode.scm"
   "../include/base/record.scm"
   "../include/base/exception.scm"
   "../include/base/prompt.scm"
   "../include/base/coroutine.scm"
   "../include/base/generator.scm"
   "../include/base/port.scm"
   "../include/base/time.scm"
   "../include/base/text.scm"
   "../include/base/io.scm"

   "../include/syntax.scm"

   "../include/compiler/high-level-ir.scm"
   "../include/compiler/backend/rkt.scm"

   "../include/nscheme/stage.scm"
   "../include/nscheme/parse.scm"
   "../include/nscheme/minimal.scm"
   "../include/nscheme/match.scm"
   "../include/nscheme/program.scm"
   "../include/nscheme/meta.scm"

   "../include/platform/common.scm"
   "../include/platform/control.scm"
   "../include/platform/io.scm"
   "../include/platform/privileged.scm"
   "../include/platform/posix/common.scm"
   "../include/platform/posix/filesystem.scm"
   "../include/platform/posix/network.scm"
   "../include/platform/posix/host-process.scm"
   "../include/platform/posix/terminal/osc.scm"
   "../include/platform/posix/terminal/csi.scm"
   "../include/platform/posix/terminal/sgr.scm"
   "../include/platform/posix/terminal/tty.scm"
   "../include/platform/posix/terminal/text.scm"
   path ...))


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
