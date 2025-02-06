(define path.here (path-directory (vector-ref (current-host-argument*) 0)))
(define (file-name->stx* fname)
  (let* ((in    (iport:file (path-append path.here fname)))
         (read* (read*/reader:data (lambda y* ((reader-track-line/start 0) (apply reader:data y*)))))
         (stx*  (read* in)))
    (iport-close in)
    stx*))
(define (file-name*->stx* fname*) (apply append (map file-name->stx* fname*)))

(define def*.base
  (file-name*->stx*
   '("../include/base/misc.scm"
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
   '("../include/platform/common.scm"
     "../include/platform/control.scm"
     "../include/platform/io.scm"
     "../include/platform/privileged.scm"
     "../include/platform/posix/common.scm"
     )))
(define def*.posix
  (file-name*->stx*
   '("../include/platform/posix/filesystem.scm"
     "../include/platform/posix/network.scm"
     "../include/platform/posix/host-process.scm"
     "../include/platform/posix/terminal/osc.scm"
     "../include/platform/posix/terminal/csi.scm"
     "../include/platform/posix/terminal/sgr.scm"
     "../include/platform/posix/terminal/tty.scm"
     "../include/platform/posix/terminal/text.scm"
     )))
