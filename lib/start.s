.section .text

.global _start
_start:
    xor %rbp, %rbp # Clear stack pointer?

    call main

    movq $60, %rax # Use _exit syscall
    movq $0, %rdi
    syscall
