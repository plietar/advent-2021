include ../common.mk

build/%.class: %.java | build
	javac -d build $^

$(BINS): build/part%: build/Part%.class
	$(call wrap,$@,java -cp build Part$*)
