# The Advent of Code, but it's a different language every day

## Solutions
- [Day 1: Sonar Sweep (Python)](day01)
- [Day 2: Dive! (Rust)](day02)
- [Day 3: Binary Diagnostic (Awk)](day03)
- [Day 4: Giant Squid (x64 Assembly)](day04)
- [Day 5: Hydrothermal Venture (Perl)](day05)
- [Day 6: Lanternfish (Nix)](day06)
- [Day 7: The Treachery of Whales (Lua)](day07)
- [Day 8: Seven Segment Search (JavaScript)](day08)
- [Day 9: Smoke Basin (PHP)](day09)
- [Day 10: Syntax Scoring (F#)](day10)
- [Day 11: Dumbo Octopus (Coq)](day11)
- [Day 13: Transparent Origami (Go)](day13)
- [Day 14: Extended Polymerization (Pony)](day14)
- [Day 16: Packet Decoder (Java)](day16)

## Languages

This is a tentative list of languages that haven't been used yet, subject to change:

- C++
- Haskell
- OCaml
- Idris
- Ruby
- Bash
- C#
- Swift
- ObjC
- Scala
- Kotlin
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

Coq does not provide a way to perform the necessary I/O used to load the input
and produce the data. In fact, there is no such thing as a Coq executable.

Instead, the Coq definitions should be extracted into OCaml, and a thin layer
of OCaml code is used to provide the necessary I/O.

As an extended goal, the solution should include a proof of termination and correctness.
