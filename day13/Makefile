include ../common.mk

$(BINS): build/%: %.go common.go
	mkdir -p build
	go build -o $@ $^
