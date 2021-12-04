.global put_long

put_long:
  push %rbp
  mov $0, %rdx
  mov %rdi, %rax
  mov $10, %rcx

  lea (print_buffer+128)(%rip), %r8
  dec %r8
  movb $'\n', (%r8)

put_long_again:
  cmp $0, %rax
  je put_long_done

  idivq %rcx
  add $'0', %rdx
  dec %r8
  mov %dl, (%r8)

  mov $0, %rdx
  
  jmp put_long_again

put_long_done:

  mov $1, %rdi
  mov %r8, %rsi
  lea (print_buffer+128)(%rip), %rdx
  sub %rsi, %rdx
  call write
  pop %rbp
  ret

.data
print_buffer: .zero 128
