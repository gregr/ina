;; racket bootstrap/run-file.rkt src/run-cli.scm test/lll.scm
(include "../src/compiler/lll.scm")
(include "../src/compiler/lll-c.scm")
(include "../src/compiler/lll-x86-64.scm")

(define (LLL-test P)
  (displayln "LLL:")
  (pretty-write P)
  (LLL-validate P)
  (displayln "==>")
  (pretty-write (LLL-eval P '()))
  (newline)
  (displayln "LLL C:")
  (LLL-emit-C P)
  (newline)
  (displayln "LLL x86-64:")
  (LLL-validate-x86-64 P)
  (LLL-emit-x86-64-at&t P)
  (newline))

(for-each
  LLL-test
  '((begin
      (set! rax 55)
      (begin
        (set! rdx 11)
        (set! rax (+ rax 7)))
      (set! rax (* rax rdx))
      (begin
        (set! r8 #x12345678)
        (set! r9 #x9ABCDEF0)
        (set! r10 #x1000)
        (set! r11 -16)
        (set! (memory 8 (+ r10 -32)) r8)
        (set! (memory 8 r10) r8)
        (set! (memory 4 (+ r10 4)) r9)
        (set! (memory 8 (+ r10 r11)) r9)
        (set! r8 (memory 2 (+ r10 4)))
        (set! r9 (memory 8 r10))
        (set! r8 (* r8 2))
        (set! r9 (* r9 2))
        (set! rdi (memory 8 (+ r10 r11)))
        (set! rsi (memory 8 (+ r10 -32)))))
    (begin
      (set! rax 7)
      "start"
      (set! rdx 8)
      (jump "end")
      "middle"
      (set! rax (+ rax rdx))
      "end"
      (set! rdx (+ rdx rax)))
    (begin
      (set! r15 "end")
      (set! rax 7)
      "start"
      (set! rdx 8)
      (jump r15)
      "middle"
      (set! rax (+ rax rdx))
      "end"
      (set! rdx (+ rdx rax)))))
