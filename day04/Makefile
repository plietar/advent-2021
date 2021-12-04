include ../common.mk

$(BINS): build/%: %.s draw.s print.s board.s read.s alloc.s
	mkdir -p build
	gcc $^ -o $@
