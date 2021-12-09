# arguments: rdi, rsi, rdx, rcx, r8, r9
# return: rax
# callee save: rbp, rbx, r12, r13, r14, r15

.global main
main:
  push %rbp

  call load_input
  call parse_draw
  mov %rax, %r12
  mov %rdx, %r13

  call board_list_parse
  mov %rax, boards(%rip)
 
again:
  cmp %r12, %r13
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

  mov $0, %rax
  pop %rbp
  ret
 
stop:
  mov $1, %rax
  pop %rbp
  ret

.data
boards: .quad 0
