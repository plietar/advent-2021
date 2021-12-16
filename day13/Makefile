include ../common.mk

$(BINS): build/%: %.go common.go | build
	go build -o $@ $^
