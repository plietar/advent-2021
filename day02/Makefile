include ../common.mk

$(BINS): build/%: %.rs common.rs
	mkdir -p build
	rustc $< -o $@
