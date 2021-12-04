.global parse_draw, draw_start, draw_end

parse_draw:
  push %r12
  lea draw_start(%rip), %r12

parse_draw_again:
  call read_int
  mov %rax, (%r12)
  addq $8, %r12
  call advance
  cmpb $'\n', %al
  jne parse_draw_again

  mov %r12, draw_end(%rip)
  pop %r12
  ret

.data
draw_start: .zero 1024
draw_end: .quad 0
