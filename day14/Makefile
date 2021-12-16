include ../common.mk

$(BINS): build/%: %/Main.pony | build
	ponyc $* -o build
