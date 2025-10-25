#lang racket/base
(provide (all-defined-out))
(module more racket/base
  (provide read-syntax-extended)
  (require racket/syntax-srcloc)
  (define read-syntax-extended
    (let ((rt.extended
           (make-readtable
            (current-readtable)
            #\#
            'dispatch-macro
            (lambda (char in . ignore*)
              (let loop ((skip 0))
                (if (eqv? (peek-char in skip) #\")
                    (let ((delim-length (+ skip 2))
                          (delim-text (string-append (read-string skip in) "##")))
                      (read-char in)
                      (let loop ((skip 0))
                        (if (and (eqv? (peek-char in skip) #\")
                                 (string=? (peek-string delim-length (+ skip 1) in) delim-text))
                            (let ((result (read-string skip in)))
                              (read-string (+ delim-length 1) in)
                              (string->bytes/utf-8 result))
                            (loop (+ skip 1)))))
                    (loop (+ skip 1))))))))
      (lambda (source in)
        (parameterize ((current-readtable rt.extended))
          (let loop ((s (read-syntax source in)))
            (define (go x)
              (cond
                ((pair?   x) (cons (loop (car x)) (loop (cdr x))))
                ((vector? x) (list->vector (map loop (vector->list x))))
                ((string? x) (string->bytes/utf-8 x))
                (else        x)))
            (if (syntax? s) (datum->syntax s (go (syntax-e s)) (syntax-srcloc s)) (go s))))))))
(require
  "primitive.rkt" "syntax-shim.rkt" 'more (for-syntax 'more)
  racket/include racket/local racket/runtime-path racket/splicing (prefix-in rkt: racket/base))
(define-syntax-rule (include-and-run path ...)
  (begin
    (include/reader path read-syntax-extended) ...
    (define-runtime-path path.here ".")
    (define library=>text* (posix-make-library=>text* #f (path-append (rkt:path->bytes path.here) #"../src")))
    (define library=>env (make-library=>env #f library=>text* (make-library=>def* #f #t library=>text*)))
    (module+ main
      (define-namespace-anchor anchor.here)
      (current-posix-argument*
       (cdr (current-posix-argument*))
       (lambda ()
         (rkt:call-with-input-file
          (bytes->string/utf-8 (car (current-posix-argument*)))
          (lambda (in)
            (let ((ns (namespace-anchor->namespace anchor.here))
                  (stx* (let loop ()
                          (let ((x (read-syntax-extended #f in)))
                            (if (eof-object? x) '() (cons x (loop)))))))
              (with-native-signal-handling
               (lambda ()
                 (with-panic-translation
                  (lambda () (rkt:for-each (lambda (stx) (rkt:eval stx ns)) stx*)))))))))))))
(include-and-run
 "../src/base/misc.scm"
 "../src/base/number.scm"
 "../src/base/list.scm"
 "../src/base/mvector.scm"
 "../src/base/vector.scm"
 "../src/base/mbytes.scm"
 "../src/base/bytes.scm"
 "../src/base/unicode.scm"
 "../src/base/prompt.scm"
 "../src/base/exception.scm"
 "../src/base/coroutine.scm"
 "../src/base/generator.scm"
 "../src/base/port.scm"
 "../src/base/text.scm"
 "../src/base/platform.scm"
 "../src/base/time.scm"
 "../src/base/io.scm"
 "../src/syntax.scm"
 "../src/compiler/high-level-ir.scm"
 "../src/compiler/high-level-passes.scm"
 "../src/compiler/backend/rkt.scm"
 "../src/compiler/target/racket.scm"
 "../src/parser/parse.scm"
 "../src/parser/minimal.scm"
 "../src/parser/program.scm"
 "../src/parser/meta.scm"
 "../src/posix/platform.scm"
 "../src/posix/signal.scm"
 "../src/posix/filesystem.scm"
 "../src/posix/network.scm"
 "../src/posix/process.scm"
 "../src/posix/cli.scm"
 "../src/posix/terminal/osc.scm"
 "../src/posix/terminal/csi.scm"
 "../src/posix/terminal/sgr.scm"
 "../src/posix/terminal/tty.scm"
 "../src/posix/terminal/text.scm"
 "../src/library.scm")
