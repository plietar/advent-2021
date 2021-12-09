.global parse_draw

parse_draw:
  push %rbp
  push %r12
  push %r13
  mov $4096, %rdi
  call alloc()
  mov %rax, %r12
  mov %rax, %r13

parse_draw_again:
  call read_int
  mov %rax, (%r13)
  addq $8, %r13
  call advance
  cmpb $'\n', %al
  jne parse_draw_again

  mov %r12, %rax
  mov %r13, %rdx

  pop %r12
  pop %r13
  pop %rbp
  ret
