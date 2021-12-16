include ../common.mk

build/%.exe: common.fs %.fs | build
	fsc --standalone -o $@ $^

$(BINS): build/%: build/%.exe
	$(call wrap,$@,mono build/$*.exe)
