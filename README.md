# The Advent of Code, but it's a different language every day

## List of languages

This is a tentative list of languages, subject to change:

- Python
- Rust
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
- Perl
- Assembly
- Prolog

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

