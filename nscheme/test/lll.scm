;; racket bootstrap/run-file.rkt src/run-cli.scm test/lll.scm | tee test/lll-snapshot.txt && git diff test/lll-snapshot.txt
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
  (LLL-validate-C P)
  (LLL-emit-C P)
  (newline)
  (displayln "LLL x86-64:")
  (LLL-validate-x86-64 P)
  (LLL-emit-x86-64-at&t P)
  (newline))

(define (LLL-test/no-eval P)
  (displayln "LLL:")
  (pretty-write P)
  (LLL-validate P)
  (newline)
  (displayln "LLL C:")
  (LLL-validate-C P)
  (LLL-emit-C P)
  (newline)
  (displayln "LLL x86-64:")
  (LLL-validate-x86-64 P)
  (LLL-emit-x86-64-at&t P)
  (newline))

(define (LLL-test/only-eval P)
  (displayln "LLL:")
  (pretty-write P)
  (LLL-validate P)
  (displayln "==>")
  (pretty-write (LLL-eval P '()))
  (newline))

(define (LLL-test-C P)
  (displayln "LLL:")
  (pretty-write P)
  (LLL-validate P)
  (newline)
  (displayln "LLL C:")
  (LLL-validate-C P)
  (LLL-emit-C P)
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
        (set! #(mloc 8 r10 -32 0 0) r8)
        (set! #(mloc 8 r10 0 0 0) r8)
        (set! #(mloc 4 r10 4 0 0) r9)
        (set! #(mloc 8 r10 0 r11 0) r9)
        (set! r8 #(mloc 2 r10 4 0 0))
        (set! r9 #(mloc 8 r10 0 0 0))
        (set! r8 (* r8 2))
        (set! r9 (* r9 2))
        (set! rdi #(mloc 8 r10 0 r11 0))
        (set! rsi #(mloc 8 r10 -32 0 0))))
    (begin
      (set! rax 55)
      (begin
        (set! rdx 11)
        (set! rax (+ rax 7)))
      (set! rax (* rax rdx))
      (begin
        (set! r8 #x12345678)
        (set! r9 #x9ABCDEF0)
        (set! r10 #x1000)
        (set! r11 -2)
        (set! #(mloc 8 r10 -32 0 0) r8)
        (set! #(mloc 8 r10 0 0 0) r8)
        (set! #(mloc 4 r10 4 0 0) r9)
        (set! #(mloc 8 r10 0 r11 3) r9)
        (set! r8 #(mloc 2 r10 4 0 0))
        (set! r9 #(mloc 8 r10 0 0 0))
        (set! r8 (* r8 2))
        (set! r9 (* r9 2))
        (set! rdi #(mloc 8 r10 0 r11 3))
        (set! rsi #(mloc 8 r10 -32 0 0))))
    (begin
      (set! rdi 0)
      (set! rax (lea #(mloc 1 0 777 rdi 0))))
    (begin
      (set! rdi 0)
      (set! rax (lea #(mloc 1 0 777 rdi 2))))
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
      "end")
    (begin
      (set! rdi 10)
      (set! rax 0)
      (jump-if (= rdi 0) "end")
      "loop"
      (set! rax (+ rax rdi))
      (set! rdi (- rdi 1))
      (jump-if (> rdi 0) "loop")
      "end")
    (begin
      (set! rdi 10)
      (set! rax 0)
      (jump-if (= rdi 0) "end")
      "loop"
      (set! rax (+ rax rdi))
      (set! rdi (- rdi 1))
      (jump-if (=/= rdi 0) "loop")
      "end")
    (begin
      (set! rdi 5)
      (set! rsi -10)
      (set! rdi (+ rdi rsi))
      (jump-if (= rdi 0) "zero")
      (jump-if (< rdi 0) "negative")
      (set! rax 1)
      (jump "end")
      "negative"
      (set! rax -1)
      (jump "end")
      "zero"
      (set! rax 0)
      (jump "end")
      "end")
    (begin
      (set! rdi 10)
      (set! rsi 11)
      (set! rax (= rdi 0))
      (set! rdx (< rdi rsi)))
    (begin
      (set! r10 #x1000)
      (set! #(mloc 8 r10 0 0 0) 33)
      (set! rax 44)
      (set! rdi rax)
      (set! rdi (+ rdi 55))
      (set! rax (atomic-cas #(mloc 8 r10 0 0 0) rax rdi)))
    (begin
      (set! r10 #x1000)
      (set! #(mloc 8 r10 0 0 0) 33)
      (set! rax #(mloc 8 r10 0 0 0))
      (set! rdi rax)
      (set! rdi (+ rdi 55))
      (set! rax (atomic-cas #(mloc 8 r10 0 0 0) rax rdi)))
    (begin
      (set! r8 #x7FFFFFFFFFFFFFF0)
      (set! r9 15)
      (set! r10 r8)
      (set! r10 (+/over r10 r9))
      (set! rax (cc over))
      (set! r11 r8)
      (set! r11 (+/carry r11 r9))
      (set! rdx (cc carry))
      (set! rcx 10)
      (set! rcx (addc rcx 100)))
    (begin
      (set! r8 #x7FFFFFFFFFFFFFF0)
      (set! r9 16)
      (set! r10 r8)
      (set! r10 (+/over r10 r9))
      (set! rax (cc over))
      (set! r11 r8)
      (set! r11 (+/carry r11 r9))
      (set! rdx (cc carry))
      (set! rcx 10)
      (set! rcx (addc rcx 100)))
    (begin
      (set! r8 #xFFFFFFFFFFFFFFF0)
      (set! r9 16)
      (set! r10 r8)
      (set! r10 (+/over r10 r9))
      (set! rax (cc over))
      (set! r11 r8)
      (set! r11 (+/carry r11 r9))
      (set! rdx (cc carry))
      (set! rcx 10)
      (set! rcx (addc rcx 100)))
    (begin
      (set! r8 #x7FFFFFFFFFFFFFF0)
      (set! r9 16)
      (set! r10 r8)
      (set! r10 (+/over r10 r9))
      (set! rax (cc over))
      (jump-if (cc over) "skip")
      (set! r11 r8)
      (set! r11 (+/carry r11 r9))
      (set! rdx (cc carry))
      "skip"
      (set! rcx 10)
      (set! rcx (addc rcx 100)))
    (begin
      (set! rdi #x7AAAAAAAAAAAAA00)
      (set! rsi #x7AAAAAAAAAAAAAAA)
      (jump-if (and rdi #xAA) "one")
      (jump-if (and rsi #xAAAA) "two")
      (set! rax 0)
      (jump "end")
      "one"
      (set! rax 1)
      (jump "end")
      "two"
      (set! rax 2)
      "end")
    (begin
      (set! rdi 5)
      (set! rsi 7)
      (set! rax (+ rdi 20))
      (set! rdx (+ rsi rax)))
    (begin
      (set! rax 77)
      (set! rdx rax)
      (set! rdi rax)
      (set! rsi rax)
      (set! rax (- 0 rax))
      (set! rdx (* rdx -1))
      (set! rdi (xor rdi -1))
      (set! rsi (xor rsi rsi))
      (set! rcx (lea #(mloc 1 rsi 17 rax 2))))
    (begin
      (set! rax #xFFFFFFFFFFFFFFFF)
      (set! rdi #xFFFFFFFFFFFFFFFF)
      (set2! rdx rax (u128* rax rdi))
      (set! r8 rdx)
      (set! r9 rax)
      (set! rax #x7FFFFFFFFFFFFFFF)
      (set! rdi #x7FFFFFFFFFFFFFFF)
      (set2! rdx rax (u128* rax rdi)))
    (begin
      (set! rax 111)
      (set! rdx 222)
      (set! rdi 333)
      (set! rsi 444)
      (set! rax (if (< rdx rdi) rsi rax)))
    (begin
      (set! rax 111)
      (set! rdx 222)
      (set! rdi 333)
      (set! rsi 444)
      (set! rax (if (= rdx 0) rsi rax)))))

(for-each
  LLL-test-C
  '((begin
      (set! rdi 1)
      (set! rsi 2)
      (set! rax (call "foo" rdi rsi))
      (set! rdi rax)
      (call r15 rdi rsi))))

(define bigadd
  '(begin
     ;; rdi = count_a
     ;; rsi = a
     ;; rdx = count_b
     ;; rcx = b
     ;; r8 = out
     (jump-if (u>= rdi rdx) "L_loop1_init")
     (set! r9 rdi)
     (set! rdi rdx)
     (set! rdx r9)
     (set! r9 rsi)
     (set! rsi rcx)
     (set! rcx r9)
     "L_loop1_init"
     (set! rax 0)
     (set! rax (+/carry rax 0))
     (jump-if (= rdx 0) "L_loop2_init")
     "L_loop1"
     (set! r9 #(mloc 8 rsi 0 rax 3))
     (set! r9 (addc r9 #(mloc 8 rcx 0 rax 3)))
     (set! #(mloc 8 r8 0 rax 3) r9)
     (set! rax (+ rax 1))
     (set! rdx (- rdx 1))
     (jump-if (=/= rdx 0) "L_loop1")
     "L_loop2_init"
     (set! rdx (cc carry))
     (set! rdi (- rdi rax))
     (jump-if (= rdi 0) "L_done")
     (set! rdx (+/carry rdx -1))
     "L_loop2"
     (set! r9 #(mloc 8 rsi 0 rax 3))
     (set! r9 (addc r9 0))
     (set! #(mloc 8 r8 0 rax 3) r9)
     (set! rax (+ rax 1))
     (set! rdi (- rdi 1))
     (jump-if (=/= rdi 0) "L_loop2")
     (set! rdx (cc carry))
     "L_done"
     (set! #(mloc 8 r8 0 rax 3) rdx)))

(LLL-test/no-eval bigadd)
