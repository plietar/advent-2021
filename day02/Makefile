include ../common.mk

$(BINS): build/%: %.rs common.rs | build
	rustc $< -o $@
