        .section        __TEXT,__text,regular,pure_instructions
        .globl  _bigadd
        .p2align        4, 0x90
_bigadd:
        #rdi = count_a
        #rsi = a
        #rdx = count_b
        #rcx = b
        #r8 = out
        shlq $3,%rdi
        shlq $3,%rdx
        cmpq %rdx,%rdi
        jae L_loop1_init
        movq %rdi,%r9
        movq %rdx,%rdi
        movq %r9,%rdx
        movq %rsi,%r9
        movq %rcx,%rsi
        movq %r9,%rcx
L_loop1_init:
        xorl %r10d,%r10d
        xorl %eax,%eax
        testq %rdx,%rdx
        jz L_loop2_init
L_loop1:
        movq (%rsi,%rax),%r9
        addq $-1,%r10
        adcq (%rcx,%rax),%r9
        setc %r10b
        movzbl %r10b,%r10d
        movq %r9,(%r8,%rax)
        addq $8,%rax
        cmpq %rdx,%rax
        jnz L_loop1
L_loop2_init:
        cmpq %rdi,%rax
        jz L_done
L_loop2:
        movq (%rsi,%rax),%r9
        addq $-1,%r10
        adcq $0,%r9
        setc %r10b
        movzbl %r10b,%r10d
        movq %r9,(%r8,%rax)
        addq $8,%rax
        cmpq %rdi,%rax
        jnz L_loop2
L_done:
        movq %r10,(%r8,%rax)
        ret
