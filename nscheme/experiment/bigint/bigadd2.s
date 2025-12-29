        .section        __TEXT,__text,regular,pure_instructions
        .globl  _bigadd
        .p2align        4, 0x90
_bigadd:
        #rdi = count_a
        #rsi = a
        #rdx = count_b
        #rcx = b
        #r8 = out
        cmpq %rdx,%rdi
        jae L_loop1_init
        movq %rdi,%r9
        movq %rdx,%rdi
        movq %r9,%rdx
        movq %rsi,%r9
        movq %rcx,%rsi
        movq %r9,%rcx
L_loop1_init:
        xorl %eax,%eax
        testq %rdx,%rdx
        jz L_loop2_init
L_loop1:
        movq (%rsi,%rax),%r9
        adcq (%rcx,%rax),%r9
        movq %r9,(%r8,%rax)
        leaq 8(%rax),%rax
        decq %rdx
        jnz L_loop1
L_loop2_init:
        setc %dl
        movzbl %dl,%edx
        shrq $3,%rax
        subq %rax,%rdi
        shlq $3,%rax
        testq %rdi,%rdi
        jz L_done
        addq $-1,%rdx
L_loop2:
        movq (%rsi,%rax),%r9
        adcq $0,%r9
        movq %r9,(%r8,%rax)
        leaq 8(%rax),%rax
        decq %rdi
        jnz L_loop2
        setc %dl
        movzbl %dl,%edx
L_done:
        movq %rdx,(%r8,%rax)
        ret
