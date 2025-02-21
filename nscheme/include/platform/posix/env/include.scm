(define (read-file path)
  (let* ((in    (iport:file path))
         (read* (read*/reader:data (lambda y* ((reader-track-line/start 0) (apply reader:data y*)))))
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
            '("platform/posix/platform.scm"
              "platform/posix/signal.scm"
              "platform/posix/filesystem.scm"
              "platform/posix/network.scm"
              "platform/posix/process.scm"
              "platform/posix/terminal/osc.scm"
              "platform/posix/terminal/csi.scm"
              "platform/posix/terminal/sgr.scm"
              "platform/posix/terminal/tty.scm"
              "platform/posix/terminal/text.scm")))
    (cons 'primitive
          (read-include-file*
            '("platform/common.scm"
              "platform/control.scm")))
    (cons 'env
          (read-include-file*
            '("platform/env/primitive.scm"
              "platform/env/evaluated.scm"
              "platform/posix/env/evaluated.scm")))))

;; TODO: move these
(define include* (read-include (path-append (path-directory (car (current-posix-argument*))) "../include")))
(define def*.base      (append* (map cdr (alist-ref include* 'base))))
(define def*.syntax    (append* (map cdr (alist-ref include* 'syntax))))
(define def*.compiler  (append* (map cdr (alist-ref include* 'compiler))))
(define def*.parser    (append* (map cdr (alist-ref include* 'parser))))
(define def*.posix     (append* (map cdr (alist-ref include* 'posix))))
(define def*.primitive (append* (map cdr (alist-ref include* 'primitive))))
(define def*.env       (append* (map cdr (alist-ref include* 'env))))
