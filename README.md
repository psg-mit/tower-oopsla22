# OOPSLA 2022 Artifact: Tower

This artifact is being submitted to support the OOPSLA'22 paper "Tower: Data Structures in Quantum Superposition" by Charles Yuan and Michael Carbin. The artifact contains:

- `README.md`, this document
- `tower.tgz`, a Docker image containing source code, pre-built binaries, and tests
- `tower.dockerfile`, a Dockerfile that generates the above image from scratch
- The directory `oopsla22-artifact`, a copy of the contents of the Docker image, containing the source code of the Tower language interpreter (`src/`) and the tests (`tests/`)

This document describes:

- The claims of the paper this artifact supports
- The kick-the-tires instructions for reviewers
- How to review the artifact for functionality
- How to review the artifact for reusability
- Detailed instructions for building the artifact

The contents of this artifact, and the sources of the Tower programming language, are available on [GitHub](https://github.com/psg-mit/tower-oopsla22).

## Claims Supported by Artifact

This artifact supports the following claims of the paper:

1. (Section 7) We implemented an interpreter for Tower in OCaml that classically executes the operational semantics of the language.

This claim is evaluated by Step 2 of the section "Validating the Paper's Claims" below. The artifact provides the source code of the Tower interpreter and a suite of test cases (from Table 1 of the paper) that the interpreter can execute.

2. (Section 8) We implemented Ground, a library of abstract data structures in Tower, and tested its operations with the interpreter.

This claim is evaluated by Steps 2 and 3 of the section "Validating the Paper's Claims" below. The artifact provides the source code of the Ground library, including all operations described in Table 1 of the paper, and a suite of tests for the correctness of the implementation that can be executed with the provided interpreter. The artifact also includes a script that validates the complexities of the data structure operations indicated by the "Complexity" column in Table 1 of the paper.

## Kick-the-Tires: Getting Started with the Artifact

First, import `tower.tgz` into Docker. In our testing, we used Docker Engine 20.10.17 on macOS Monterey. The image may be loaded using the following commands:

```shell
$ docker load < tower.tgz
Loaded image: oopsla22-artifact:latest
$ docker run -it --rm oopsla22-artifact bash
opam@502e0da78017:~/oopsla22-artifact$
```

The artifact is in the `oopsla22-artifact/` directory inside the user home directory, inside which the interpreter for Tower is already installed as `./tower`.

```shell
$ cd ~/oopsla22-artifact/
$ ./tower -help
The interpreter for the Tower quantum programming language.
Usage: ./tower <options> [program]
Options are:
  -v Run in verbose mode.
  -i Run the classical interpreter.
  --interp_lir Run the LIR interpreter.
  --interp_circuit Run the circuit interpreter.
  --interp_prim Run the primitive circuit interpreter.
  --inline Run the inlining pass before the interpreter.
  -c Compile the program to a circuit.
  -p Print the compiler and interpreter outputs.
  -b Set the bound for a given function.
  --no-prim Do not instantiate primitive circuits.
  --word_size Set the word size.
  -help  Display this list of options
  --help  Display this list of options
```

To quickly check that the artifact is functional, execute the interpreter on the `length` example:

```shell
$ ./tower -i tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
Exited normally.
```

## Functionality: Validating the Paper's Claims

The easiest way to validate the paper's claims is to do the following in the `oopsla22-artifact/` directory:

1. Ensure the interpreter `./tower` is present.

If it is not, rebuild it from source by invoking `make` (for more instructions, see the "Detailed Build Instructions" section).

2. Execute the interpreter on the data structure correctness tests by invoking `make check`.

This operation should take about one minute to complete.

To evaluate the claim, verify that the output of each line is `Exited normally.`, indicating that the test passes.

The `tests/` directory contains a set of `.twr` programs corresponding to the benchmarks in the paper. Specifically, the programs whose name ends in `_tests` contain tests for the correctness of the data structure implementations.

3. Execute the data structure performance tests using `./bench.sh`.

This operation should take about three minutes to complete. A saved reference copy of the output of `./bench.sh` is stored in the file `bench.out`.

The `./bench.sh` script lists each data structure operation from Table 1 of the paper, followed by its expected qubit and gate counts as a function of the structure size.
It then computes and prints the expected and actual qubit and gate counts for the operation, instantiated at different sizes.

To evaluate the claim, verify that each line of output that compares the actual and expected qubit and gate counts states `PASS`, indicating that the actual complexity matches Table 1 of the paper.

The qubit and gate counts output by `./bench.sh` will be consistent with the asymptotic complexities reported in the "Complexity" column in Table 1 of the paper, which subsume both qubit and gate complexities.
For convenience, this column from the paper is reproduced below, alongside the qubit and gate counts that are output by `./bench.sh`.

| Paper Benchmark  | Filename          | Complexity (Tab. 1) | Qubits          | Gates              |
| ---------------- | ----------------- | ------------------- | --------------- | ------------------ |
| List             |                   |                     |                 |                    |
| - length         | `length.twr`      | O(n)                | 34n + 32        | 23n + 3            |
| - sum            | `sum.twr`         | O(n)                | 34n + 40        | 21n + 3            |
| - find_pos       | `find_pos.twr`    | O(n)                | 42n + 31        | 19n + 3            |
| - remove         | `remove.twr`      | O(n)                | 26n + 56        | 42n + 3            |
| Stack (list)     | `stack.twr`       |                     |                 |                    |
| - push_front     |                   | O(1)                | 40              | 4                  |
| - pop_front      |                   | O(1)                | 48              | 4                  |
| Queue (list)     | `queue.twr`       |                     |                 |                    |
| - push_back      |                   | O(n)                | 34n + 32        | 24n                |
| - pop_front      |                   | O(1)                | 48              | 4                  |
| String (word)    | `string_word.twr` |                     |                 |                    |
| - is_empty       |                   | O(1)                | 25              | 3                  |
| - length         |                   | O(1)                | 24              | 1                  |
| - get_prefix     |                   | O(k)                | 11k             | 52                 |
| - get_substring  |                   | O(k)                | 12k             | 54                 |
| - get            |                   | O(k)                | 6k + 1          | 19                 |
| - is_prefix      |                   | O(poly(k))          | k^2 + 11k       | 98k + 3            |
| - num_matching   |                   | O(poly(k))          | k^2 + 13k + 4   | 110k + 127         |
| - equal          |                   | O(k)                | 6k + 3          | 5                  |
| - concat         |                   | O(k)                | 11k             | 8                  |
| - compare        |                   | O(poly(k))          | 5k^2 + 12k      | 108k + 3           |
| Set (radix tree) | `radix_tree.twr`  |                     |                 |                    |
| - insert         |                   | O(poly(k))          | 13k^2 + 21k + 9 | 1440k^2 + 5056k    |
| - contains       |                   | O(poly(k))          | 17k^2 + 18k + 2 | 784k^2 + 1612k + 1 |
| Set (hash table) | `hash_table.twr`  |                     |                 |                    |
| - insert         |                   | O(n)                | 52n + 72        | 68n + 15           |
| - contains       |                   | O(n)                | 52n + 81        | 136n + 39          |

## Reusability: Writing Tower Programs

The Tower interpreter included in the artifact makes it possible to write new Tower programs and extend the functionality of the language.
The file `src/README.md` describes the organization of the Tower interpreter sources, should you wish to modify the code. The source code of Tower is also available on [GitHub](https://github.com/psg-mit/tower-oopsla22).

To execute a Tower program, supply it as an argument to the interpreter, preceded by the `-i` flag and all requisite library files in appropriate order, for example:

```shell
$ ./tower -i tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
Exited normally.
```

The interpreter will execute the program. If it encounters a runtime error, it aborts program execution and prints the error. Additional verbosity, such as the final value returned by the program, may be obtained using the `-v` and `-p` flags, as described by the `-help` flag.

The provided examples in `tests/` are a convenient way to understand the syntax of Tower. Each program is a series of functions declared using the `fun` keyword. To execute the program, there must be one function declared with the signature `fun main() -> unit`.

In addition to the classical interpreter, this artifact includes a compiler from a Tower program to a quantum circuit. This compiler may be invoked using the `-c` flag and the resulting circuit printed using the `-p` flag. Please see the `Makefile` for more examples of invoking the Tower compiler.

## Detailed Build Instructions

In the provided image, simply run `make` within `oopsla22-artifact` to build the interpreter from source.

To build the interpreter outside of the image, first install OCaml version 4.14 and the libraries `core`, `menhir`, and `ppx_deriving`.
The recommended way to install OCaml and the dependent libraries is via [OPAM](http://opam.ocaml.org).
Then, to build the interpreter `./tower`, simply run `make`.
