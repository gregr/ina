#lang racket/base
(provide (all-defined-out))
(require
  ;; Toggle commenting on these two to turn on interrupt-aware lambdas.
  "../platform/racket/nscheme.rkt"
  ;(rename-in "../platform/racket/nscheme.rkt" (interruptible-lambda lambda))
  racket/include racket/local racket/splicing (prefix-in rkt: racket/base))
(include "../src/base/misc.scm")
(include "../src/base/list.scm")
(include "../src/base/number.scm")
(include "../src/base/mvector.scm")
(include "../src/base/vector.scm")
(include "../src/base/mbytevector.scm")
(include "../src/base/bytevector.scm")
(include "../src/base/unicode.scm")
(include "../src/base/prompt.scm")
(include "../src/base/exception.scm")
(include "../src/base/coroutine.scm")
(include "../src/base/generator.scm")
(include "../src/base/port.scm")
(include "../src/base/text.scm")
(include "../src/base/platform.scm")
(include "../src/base/time.scm")
(include "../src/base/io.scm")
(include "../src/syntax.scm")
(include "../src/compiler/high-level-ir.scm")
(include "../src/compiler/backend/rkt.scm")
(include "../src/parser/stage.scm")
(include "../src/parser/parse.scm")
(include "../src/parser/minimal.scm")
(include "../src/parser/match.scm")
(include "../src/parser/program.scm")
(include "../src/parser/meta.scm")
(include "../src/posix/platform.scm")
(include "../src/posix/signal.scm")
(include "../src/posix/filesystem.scm")
(include "../src/posix/network.scm")
(include "../src/posix/process.scm")
(include "../src/posix/terminal/osc.scm")
(include "../src/posix/terminal/csi.scm")
(include "../src/posix/terminal/sgr.scm")
(include "../src/posix/terminal/tty.scm")
(include "../src/posix/terminal/text.scm")
(include "../src/env.scm")
(define path.library  (path-append (current-posix-program-directory) "../src"))
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
