.global board_parse, board_debug, board_list_parse, board_list_debug, board_mark, board_list_mark, board_score, board_wins, board_list_take_winners
.equ BOARD_DIMENSION, 5
.equ BOARD_SIZE, (BOARD_DIMENSION * BOARD_DIMENSION * 8 + 8)
.equ board_link, (BOARD_DIMENSION * BOARD_DIMENSION * 8)

# board* board_new()
board_new:
  push %rbp
  mov $BOARD_SIZE, %rdi
  call alloc
  movq $0, board_link(%rax)
  pop %rbp
  ret

# board* board_parse()
board_parse:
  push %rbp
  push %r12
  push %r13

  call board_new
  mov %rax, %r12
  mov $0, %r13

board_parse_again:
  cmp $(BOARD_DIMENSION*BOARD_DIMENSION), %r13
  je board_parse_done
  
  call skip_whitespace
  call read_int
  mov %rax, (%r12,%r13,8)
  inc %r13
  jmp board_parse_again

board_parse_done:
  mov %r12, %rax
  pop %r13
  pop %r12
  pop %rbp
  ret

# void board_debug(board*)
board_debug:
  push %rbp
  push %r12
  push %r13
  mov %rdi, %r12
  lea board_link(%rdi), %r13

board_debug_again:
  cmp %r12, %r13
  je board_debug_done

  leaq fmt_board_row(%rip), %rdi
  mov (%r12), %rsi
  mov 8(%r12), %rdx
  mov 16(%r12), %rcx
  mov 24(%r12), %r8
  mov 32(%r12), %r9
  call _printf
  add $40, %r12
  jmp board_debug_again

board_debug_done:
  pop %r13
  pop %r12
  pop %rbp
  ret

# board* board_list_parse()
board_list_parse:
  push %rbp
  push %r12
  push %r13

  mov $0, %r12

board_list_parse_again:
  call skip_whitespace
  call is_eof
  cmp $1, %rax
  je board_list_parse_done

  call board_parse
  cmp $0, %r12
  je board_list_parse_first

  mov %rax, board_link(%r13) 
  mov %rax, %r13
  jmp board_list_parse_again

board_list_parse_first:
  mov %rax, %r12
  mov %rax, %r13
  jmp board_list_parse_again

board_list_parse_done:
  mov %r12, %rax
  pop %r13
  pop %r12
  pop %rbp
  ret

# void board_list_parse(board*)
board_list_debug:
  push %r12
  mov %rdi, %r12

board_list_debug_again:
  cmp $0, %r12
  je board_list_debug_done

  mov %r12, %rdi
  call board_debug
  leaq fmt_nl(%rip), %rdi
  call _printf

  mov board_link(%r12), %r12
  jmp board_list_debug_again

board_list_debug_done:
  pop %r12
  ret

# void board_mark(board*, uint64_t)
board_mark:
  lea board_link(%rdi), %rdx

board_mark_again:
  cmp %rdi, %rdx
  je board_mark_done
  cmp (%rdi), %rsi
  jne board_mark_skip
  negq (%rdi)

board_mark_skip:
  add $8, %rdi
  jmp board_mark_again

board_mark_done:
  ret

# void board_list_mark(board*, uint64_t)
board_list_mark:
  push %rbp
  push %r12
  push %r13
  mov %rdi, %r12
  mov %rsi, %r13

board_list_mark_again:
  cmp $0, %r12
  je board_list_mark_done

  mov %r12, %rdi
  mov %r13, %rsi
  call board_mark

  mov board_link(%r12), %r12
  jmp board_list_mark_again

board_list_mark_done:
  pop %r13
  pop %r12
  pop %rbp
  ret

# uint64_t board_score(board*)
board_score:
  lea board_link(%rdi), %rsi
  mov $0, %rax

board_score_again:
  cmp %rdi, %rsi
  je board_score_done

  cmpq $0, (%rdi)
  jl board_score_skip
  add (%rdi), %rax

board_score_skip:
  add $8, %rdi
  jmp board_score_again

board_score_done:
  ret

# bool board_wins(board*)
board_wins:
  push %rbp
  push %r12
  push %r13
  mov %rdi, %r12
  mov $0, %r13

  lea (%r12), %rdi
  call board_check_row
  orq %rax, %r13

  lea 40(%r12), %rdi
  call board_check_row
  orq %rax, %r13

  lea 80(%r12), %rdi
  call board_check_row
  orq %rax, %r13

  lea 120(%r12), %rdi
  call board_check_row
  orq %rax, %r13

  lea 160(%r12), %rdi
  call board_check_row
  orq %rax, %r13

  lea (%r12), %rdi
  call board_check_column
  orq %rax, %r13

  lea 8(%r12), %rdi
  call board_check_column
  orq %rax, %r13

  lea 16(%r12), %rdi
  call board_check_column
  orq %rax, %r13

  lea 24(%r12), %rdi
  call board_check_column
  orq %rax, %r13

  lea 32(%r12), %rdi
  call board_check_column
  orq %rax, %r13

  mov %r13, %rax
  pop %r13
  pop %r12
  pop %rbp
  ret

# bool board_check_row(uint64_t* data)
board_check_row:
  cmpq $0, (%rdi)
  jg board_check_row_no
  cmpq $0, 8(%rdi)
  jg board_check_row_no
  cmpq $0, 16(%rdi)
  jg board_check_row_no
  cmpq $0, 24(%rdi)
  jg board_check_row_no
  cmpq $0, 32(%rdi)
  jg board_check_row_no
  mov $1, %rax
  ret
board_check_row_no:
  mov $0, %rax
  ret

# bool board_check_column(uint64_t* data)
board_check_column:
  cmpq $0, 0(%rdi)
  jg board_check_column_no
  cmpq $0, 40(%rdi)
  jg board_check_column_no
  cmpq $0, 80(%rdi)
  jg board_check_column_no
  cmpq $0, 120(%rdi)
  jg board_check_column_no
  cmpq $0, 160(%rdi)
  jg board_check_column_no
  mov $1, %rax
  ret
board_check_column_no:
  mov $0, %rax
  ret

# void board_list_take_winners(board **)
board_list_take_winners:
  push %r12
  push %r13
  push $0
  mov %rdi, %r12
  mov %rsp, %r13

board_list_take_winners_again:
  cmpq $0, (%r12)
  je board_list_take_winners_no

  mov (%r12), %rdi
  call board_wins
  cmp $1, %rax
  je board_list_take_winners_yes

  mov (%r12), %r12
  lea board_link(%r12), %r12
  jmp board_list_take_winners_again

board_list_take_winners_yes:
  mov (%r12), %rax
  mov %rax, (%r13)

  mov board_link(%rax), %rdx
  movq $0, board_link(%rax)
  mov %rdx, (%r12)
  jmp board_list_take_winners_again

  pop %r13
  pop %r12
  pop %rbp
  ret

board_list_take_winners_no:
  pop %rax
  pop %r13
  pop %r12
  ret

.data
fmt_board_row: .asciz "%d %d %d %d %d\n"
fmt_nl: .asciz "\n"
