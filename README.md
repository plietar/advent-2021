# The Advent of Code, but it's a different language every day

## Solutions
- [Day 1: Sonar Sweep (Python)](day01)
- [Day 2: Dive! (Rust)](day02)
- [Day 3: Binary Diagnostic (Awk)](day03)
- [Day 4: Giant Squid (x64 Assembly)](day04)
- [Day 5: Hydrothermal Venture (Perl)](day05)

## Languages

This is a tentative list of languages that haven't been used yet, subject to change:

- JS
- Java
- C++
- Haskell
- Pony
- Coq
- OCaml
- Idris
- Ruby
- Bash
- Go
- C#
- Lua
- Swift
- ObjC
- Scala
- Kotlin
- PHP
- F#
- Nix
- Prolog

## Structure

Every day of the advent calendar is located in a separate directory. Each
directory contains a `Makefile` with build instructions suitable to the
specific language. The `Makefile` should expose two targets, `build/part1` and
`build/part2`, each one of them an executable file that solves the respective
parts of the problem. Each executable reads the input data on stdin and
produces the output on stdout.

Additionally, each subdirectory contains a `default.nix`, describing the
dependencies necessary to build or execute the solution.

## Rules

Solutions may only use the standard library that comes with the default
installation, with some exceptions.

### Assembly

The assembly solution may use the `read`, `write` and `mmap` functions from the
C runtime. The first two are necessary in order to load the input data and
produce the output, the latter is used to allocate memory. On Linux, we could
have replaced the method calls with system calls, but that is not portable and
not officially supported on macOS.

### Coq

Out of the box, Coq does not provide a way to perform the necessary I/O used to
load the input and produce the data. In fact, there is no such thing as a Coq
executable. [Coq-io](https://github.com/coq-io/io) will be used to extract the
Coq definitions to OCaml and expose I/O primitives, allowing an executable to
be produced.

As an extended goal, the solution should include a proof of correctness.

