include ../common.mk

$(BINS): build/%: %.idr | build
	idris2 $< -o hello 
