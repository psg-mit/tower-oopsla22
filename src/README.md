# Tower

This directory contains the source code for the Tower interpreter and compiler in OCaml. The Tower source is organized as follows.

The main sources of Tower are in `src/`:

- `dune` specifies OCaml build dependencies and flags.
- `parser.mly` and `lexer.mll` are a Menhir lexer/parser for the Tower syntax.
- `main.ml` is the entry point of the interpreter.
- `test/` includes unit tests for the compiler.

The `lib/` subdirectory includes the modules of the Tower interpreter and compiler:

- `alloc/` implements the allocation of logical qubit registers.
- `ast/` defines the Tower abstract syntax tree and utility functions.
- `circuit/`, `ir/` and `lir/` define intermediate representations used by the Tower compiler.
- `codegen/` defines the quantum assembly representation emitted by the Tower compiler.
- `errors/` defines static and runtime errors in Tower.
- `interp/` implements the classical Tower interpreter.
- `symbol/` implements the compiler symbol table.
- `args.ml` defines command-line flags.

Other files are OCaml interface files and dependent modules of the above components of the interpreter and compiler.
