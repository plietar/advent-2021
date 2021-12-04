.global advance, peek, read_int, load_input, skip_whitespace, is_eof
.equ BUFFER_SIZE, 8192

# void load_input()
load_input:
  push %rbp

  mov $BUFFER_SIZE, %rdi
  call alloc
  mov %rax, cursor(%rip)

  mov $0, %rdi
  mov %rax, %rsi
  mov $BUFFER_SIZE, %rdx
  call _read

  pop %rbp
  cmpq $BUFFER_SIZE, %rax
  je _abort

  add cursor(%rip), %rax
  mov %rax, buffer_end(%rip)
  ret

# uint64 read_int()
read_int:
  mov $0, %rax
  mov cursor(%rip), %rdx

read_int_loop:
  mov (%rdx), %bl
  sub $'0', %bl
  jl read_int_done
  imul $10, %rax
  add %rbx, %rax
  inc %rdx
  jmp read_int_loop

read_int_done:
  mov %rdx, cursor(%rip)
  ret

# char advance()
advance:
  mov cursor(%rip), %rax
  movb (%rax), %al
  incq cursor(%rip)
  ret

# void skip_whitespace()
skip_whitespace:
  mov cursor(%rip), %rax

skip_whitespace_loop:
  cmp %rax, buffer_end(%rip)
  je skip_whitespace_done
  cmpb $' ', (%rax)
  je skip_whitespace_match
  cmpb $'\n', (%rax)
  je skip_whitespace_match

skip_whitespace_done:
  mov %rax, cursor(%rip)
  ret

skip_whitespace_match:
  inc %rax
  jmp skip_whitespace_loop

is_eof:
  mov cursor(%rip), %rax
  cmp %rax, buffer_end(%rip)
  je is_eof_yes
  mov $0, %rax
  ret

is_eof_yes:
  mov $1, %rax
  ret

.data
cursor: .quad 0
buffer_end: .quad 0
