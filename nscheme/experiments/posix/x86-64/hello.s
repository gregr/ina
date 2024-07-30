  .global _main

  .data
  .align 16
msg:
  .ascii "Hello, world, with syscall!\n"
len = . - msg  /* current address - address of msg */

  .text
_main:
  /* write to stdout */
  movq    $0x2000004, %rax
  movq    $1, %rdi
  leaq    msg(%rip), %rsi
  movq    $len, %rdx
  syscall
  /* exit */
  movq    $0x2000001, %rax
  movq    $0, %rdi
  syscall
