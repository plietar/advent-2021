# arguments: rdi, rsi, rdx, rcx, r8, r9
# return: rax
# callee save: rbp, rbx, r12, r13, r14, r15

.global _main
_main:
  push %rbp

  call load_input
  call parse_draw
  call board_list_parse
  mov %rax, boards(%rip)

  lea draw_start(%rip), %r12
 
again:
  cmp draw_end(%rip), %r12
  je stop

  mov boards(%rip), %rdi
  mov (%r12), %rsi
  call board_list_mark

  lea boards(%rip), %rdi
  call board_list_take_winners
  cmp $0, %rax
  jne found

  addq $8, %r12
  jmp again

found:
  mov %rax, %rdi
  call board_score

  imul (%r12), %rax
  mov %rax, %rdi
  call put_long

  mov $0, %rdi
  call _exit
 
stop:
  mov $1, %rdi
  call _exit

.data
boards: .quad 0
