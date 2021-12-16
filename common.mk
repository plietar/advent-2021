BINS:=build/part1 build/part2
RUNS:=$(patsubst build/%,run-%,$(BINS))
INPUT:=example.txt

all: $(BINS)
clean:
	rm -rf build/

$(RUNS): run-%: build/%
	./$< < $(INPUT)

build:
	mkdir -p build

run-all: $(RUNS)

define wrap
	echo -e "#!/usr/bin/env bash\nexec $(2) $$@" > $1
	chmod +x $1
endef

.PHONY: all clean run-all $(RUNS)
