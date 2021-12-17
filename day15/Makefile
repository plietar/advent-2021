include ../common.mk

$(BINS): build/%: %.idr | build
	idris2 --codegen refc $< -o hello 
