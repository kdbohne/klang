.section .text

.global syscall5
syscall5:
    movq %rdi, %rax # syscall number
    movq %rsi, %rdi # arg1
    movq %rdx, %rsi # arg2
    movq %rcx, %rdx # arg3
    movq %r8,  %r10 # arg4
    movq %r9,  %r8  # arg5

    syscall
    ret

/*
.global _start
_start:
    xor %rbp, %rbp
    and -16, %rsp
    call main
    ret
*/
/*
_start:
    xor %rbp, %rbp
    pop %rdi
    movq %rsp, %rsi
    and %rsp, -16
    call main

    movq %rax, %rdi
    movq $60, %rax
    syscall

    ret
*/
