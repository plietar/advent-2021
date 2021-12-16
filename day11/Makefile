include ../common.mk

build/Main.vo build/solver.ml build/solver.mli: Main.v
	mkdir -p build
	coqc -o build/Main.vo $^

build/solver.cmi: build/solver.mli
	ocamlc -c build/solver.mli

build/solver.cmo: build/solver.ml build/solver.cmi
	ocamlc -I build -c build/solver.ml

build/%.cmo: %.ml
	ocamlc -I build -o $@ -c $<

build/common.cmo: build/solver.cmo
build/part1.cmo: build/common.cmo
build/part2.cmo: build/common.cmo

$(BINS): build/%: build/solver.cmo build/common.cmo build/%.cmo
	mkdir -p build
	ocamlc -I build -o $@ $^
