include ../common.mk
OBJS += build/main.exe

build/main.exe: main.fs
	mkdir -p build
	fsc --standalone -o $@ $^

$(BINS): build/%: build/main.exe run.sh
	mkdir -p build
	ln -sf ../run.sh $@
