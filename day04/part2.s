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
  cmp %r13, %r12
  je stop
  cmpq $0, boards(%rip)
  je stop

  mov boards(%rip), %rdi
  mov (%r12), %rsi
  call board_list_mark

  lea boards(%rip), %rdi
  call board_list_take_winners

  cmpq $0, %rax
  je not_found

  mov %rax, chosen_board(%rip)
  mov (%r12), %rbx
  mov %rbx, chosen_draw(%rip)

not_found:
  addq $8, %r12
  jmp again

stop:
  mov chosen_board(%rip), %rdi
  call board_score

  imul chosen_draw(%rip), %rax
  mov %rax, %rdi
  call put_long

  mov $0, %rdi
  call exit

.data
boards: .quad 0
chosen_board: .quad 0
chosen_draw: .quad 0
