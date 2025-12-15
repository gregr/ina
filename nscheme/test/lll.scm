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
        (set! #(mloc 8 r10 -32 0) r8)
        (set! #(mloc 8 r10 0 0) r8)
        (set! #(mloc 4 r10 4 0) r9)
        (set! #(mloc 8 r10 0 r11) r9)
        (set! r8 #(mloc 2 r10 4 0))
        (set! r9 #(mloc 8 r10 0 0))
        (set! r8 (* r8 2))
        (set! r9 (* r9 2))
        (set! rdi #(mloc 8 r10 0 r11))
        (set! rsi #(mloc 8 r10 -32 0))))
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
      (set! rdx (+ rdx rax)))
    (begin
      (set! rcx 3)
      (set! rax 127)
      (set! rdx -64)
      (set! rdi rdx)
      (set! rsi rdx)
      (set! rax (and rax #x3f))
      (set! r8 rax)
      (set! r9 rax)
      (set! r8 (asr r8 3))
      (set! r9 (lsr r9 2))
      (set! rdx (asr rdx 2))
      (set! rdi (lsr rdi rcx))
      (set! rsi (asl rsi 2)))
    (begin
      (set! rdi 10)
      (set! rax 0)
      "loop"
      (jump-if (= rdi 0) "end")
      (set! rax (+ rax rdi))
      (set! rdi (- rdi 1))
      (jump "loop")
      "end")))
