include ../common.mk

build/%: %.rs common.rs
	mkdir -p build
	rustc $< -o $@
