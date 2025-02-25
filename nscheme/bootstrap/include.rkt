#lang racket/base
(provide (all-defined-out))
(require
  ;; Toggle commenting on these two to turn on interrupt-aware lambdas.
  "../platform/racket/nscheme.rkt"
  ;(rename-in "../platform/racket/nscheme.rkt" (interruptible-lambda lambda))
  racket/include racket/local racket/splicing (prefix-in rkt: racket/base))
(include "../include/base/misc.scm")
(include "../include/base/pair.scm")
(include "../include/base/list.scm")
(include "../include/base/number.scm")
(include "../include/base/mvector.scm")
(include "../include/base/vector.scm")
(include "../include/base/mbytevector.scm")
(include "../include/base/bytevector.scm")
(include "../include/base/unicode.scm")
(include "../include/base/prompt.scm")
(include "../include/base/exception.scm")
(include "../include/base/coroutine.scm")
(include "../include/base/generator.scm")
(include "../include/base/port.scm")
(include "../include/base/text.scm")
(include "../include/base/platform.scm")
(include "../include/base/time.scm")
(include "../include/base/io.scm")
(include "../include/syntax.scm")
(include "../include/compiler/high-level-ir.scm")
(include "../include/compiler/backend/rkt.scm")
(include "../include/parser/stage.scm")
(include "../include/parser/parse.scm")
(include "../include/parser/minimal.scm")
(include "../include/parser/match.scm")
(include "../include/parser/program.scm")
(include "../include/parser/meta.scm")
(include "../include/posix/platform.scm")
(include "../include/posix/signal.scm")
(include "../include/posix/filesystem.scm")
(include "../include/posix/network.scm")
(include "../include/posix/process.scm")
(include "../include/posix/terminal/osc.scm")
(include "../include/posix/terminal/csi.scm")
(include "../include/posix/terminal/sgr.scm")
(include "../include/posix/terminal/tty.scm")
(include "../include/posix/terminal/text.scm")
(include "../include/env.scm")
(define path.library  (posix-default-library-path))
(define library=>def* (posix-make-library=>def* path.library))
(define library=>env  (make-library=>env/library=>def* library=>def* eval-definition*))
(define env.medium    (alist-ref library=>env 'medium))
(define env.large     (alist-ref library=>env 'large))

(define-syntax-rule (nscheme-run stx ...)
  (with-native-signal-handling (lambda () stx ...)))


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
