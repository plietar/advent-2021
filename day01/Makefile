BINS=build/part1 build/part2
all: $(BINS)
clean:
	rm -f $(BINS)

build/%:
	mkdir -p build
	ln -sf ../main.py $@

.PHONY: all clean

