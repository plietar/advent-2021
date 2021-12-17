include ../common.mk

$(BINS): build/%: %.idr | build
	idris2 --profile $< -o hello 
