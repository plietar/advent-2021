include ../common.mk

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	CFLAGS += -Dprintf=_printf -D write=_write -D read=_read -D abort=_abort -Dmmap=_mmap -Dmain=_main -Dexit=_exit
endif

$(BINS): build/%: %.s draw.s print.s board.s read.s alloc.s | build
	$(CC) $(CFLAGS) $^ -o $@
