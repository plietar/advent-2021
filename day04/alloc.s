.global alloc, alloc_big, alloc_debug

.equ SLAB_SIZE, 0x1000
.equ MAP_ANONYMOUS, 4096
.equ MAP_PRIVATE, 2
.equ PROT_NONE, 0
.equ PROT_READ, 1
.equ PROT_WRITE, 2

# void *alloc(size_t)
alloc:
  # Large allocations go straight to mmap
  cmpq $SLAB_SIZE, %rdi
  jge alloc_big

  # If requested size fits in the current chunk, just use that
  mov alloc_end(%rip), %rsi
  sub alloc_start(%rip), %rsi
  cmpq %rdi, %rsi
  jge alloc_fast_path

  # Allocate a new slab from the OS.
  push %rdi
  mov $SLAB_SIZE, %rdi
  call alloc_big
  mov %rax, alloc_start(%rip)
  add $SLAB_SIZE, %rax
  mov %rax, alloc_end(%rip)
  pop %rdi

alloc_fast_path:
  mov alloc_start(%rip), %rax
  add %rdi, alloc_start(%rip)
  ret

# void *alloc_big(size_t)
alloc_big:
  push %rbp
  mov %rdi, %rsi
  mov $0, %rdi
  mov $(PROT_READ+PROT_WRITE), %rdx
  mov $(MAP_PRIVATE+MAP_ANONYMOUS), %rcx
  mov $0, %r8
  mov $0, %r9
  call _mmap
  pop %rbp
  ret

# void alloc_debug()
alloc_debug:
  push %rbp
  leaq fmt_alloc_debug(%rip), %rdi
  mov alloc_start(%rip), %rsi
  mov alloc_end(%rip), %rdx
  call _printf
  pop %rbp
  ret


.data
alloc_start: .quad 0x0
alloc_end: .quad 0x0
fmt_alloc_debug: .asciz "start=%p end=%p\n"
