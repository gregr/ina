(define (read-file path)
  (let* ((in    (iport:file path))
         (read* (read*/reader:data ((reader:data-track-line/start 0) reader:data)))
         (stx*  (read* in)))
    (iport-close in)
    stx*))
(define (read-file* path*) (append* (map read-file path*)))
(define (read-include path.include)
  (define (read-include-file* p*)
    (map (lambda (p) (let ((p (path->bytevector p)))
                       (cons p (read-file (path-append path.include p)))))
         p*))
  (list (cons 'base
          (read-include-file*
            '("base/misc.scm"
              "base/pair.scm"
              "base/list.scm"
              "base/number.scm"
              "base/mvector.scm"
              "base/vector.scm"
              "base/mbytevector.scm"
              "base/bytevector.scm"
              "base/unicode.scm"
              "base/prompt.scm"
              "base/exception.scm"
              "base/coroutine.scm"
              "base/generator.scm"
              "base/port.scm"
              "base/text.scm"
              "base/platform.scm"
              "base/time.scm"
              "base/io.scm")))
    (cons 'syntax
          (read-include-file*
            '("syntax.scm")))
    (cons 'compiler
          (read-include-file*
            '("compiler/high-level-ir.scm"
              "compiler/backend/rkt.scm")))
    (cons 'parser
          (read-include-file*
            '("parser/stage.scm"
              "parser/parse.scm"
              "parser/minimal.scm"
              "parser/match.scm"
              "parser/program.scm"
              "parser/meta.scm")))
    (cons 'posix
          (read-include-file*
            '("posix/platform.scm"
              "posix/signal.scm"
              "posix/filesystem.scm"
              "posix/network.scm"
              "posix/process.scm"
              "posix/terminal/osc.scm"
              "posix/terminal/csi.scm"
              "posix/terminal/sgr.scm"
              "posix/terminal/tty.scm"
              "posix/terminal/text.scm")))
    (cons 'primitive
          (read-include-file*
            '("platform/common.scm"
              "platform/control.scm")))
    (cons 'env
          (read-include-file*
            '("platform/env/primitive.scm"
              "platform/env/evaluated.scm"
              "posix/env/evaluated.scm")))))

;; TODO: move these
(define include* (read-include (path-append (path-directory (car (current-posix-argument*))) "../include")))
(define def*.base      (append* (map cdr (alist-ref include* 'base))))
(define def*.syntax    (append* (map cdr (alist-ref include* 'syntax))))
(define def*.compiler  (append* (map cdr (alist-ref include* 'compiler))))
(define def*.parser    (append* (map cdr (alist-ref include* 'parser))))
(define def*.posix     (append* (map cdr (alist-ref include* 'posix))))
(define def*.primitive (append* (map cdr (alist-ref include* 'primitive))))
(define def*.env       (append* (map cdr (alist-ref include* 'env))))
