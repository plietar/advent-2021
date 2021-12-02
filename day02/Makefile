BINS=build/part1 build/part2
all: $(BINS)
clean:
	rm -f $(BINS)

build/%: %.rs common.rs
	mkdir -p build
	rustc $< -o $@

.PHONY: all clean
