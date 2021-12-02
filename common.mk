BINS:=build/part1 build/part2
RUNS:=$(patsubst build/%,run-%,$(BINS))
INPUT:=example.txt

all: $(BINS)
clean:
	rm -f $(BINS)

$(RUNS): run-%: build/%
	./$< < $(INPUT)

run-all: $(RUNS)

.PHONY: all clean run-all $(RUNS)
