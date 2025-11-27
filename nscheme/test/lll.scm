;; racket bootstrap/run-file.rkt src/run-cli.scm test/lll.scm
(include "../src/compiler/lll.scm")
(include "../src/compiler/lll-c.scm")
(include "../src/compiler/lll-x86-64.scm")

(let ((P '(begin
            (set! rax 55)
            (begin
              (set! rdx 11)
              (set! rax (+ rax 7)))
            (set! rax (* rax rdx)))))
  (displayln "LLL:")
  (pretty-write P)
  (LLL-validate P)
  (displayln "==>")
  (writeln (LLL-eval P '()))
  (newline)
  (displayln "LLL C:")
  (LLL-validate-C P)
  (LLL-emit-C P)
  (newline)
  (displayln "LLL x86-64:")
  (LLL-validate-x86-64 P)
  (LLL-emit-x86-64-at&t P))
