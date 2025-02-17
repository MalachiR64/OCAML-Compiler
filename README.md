# OCaml Compiler

A simple compiler for an ML-like functional language, implemented in OCaml. This project includes a lexer, parser, type checker, and interpreter, demonstrating key compiler construction techniques.

## Table of Contents
- [Project Structure](#project-structure)
- [Syntax Overview](#syntax-overview)
- [Example Test Cases](#example-test-cases)
- [Building and Running](#building-and-running)
- [Future Improvements](#future-improvements)
- [Contributors](#contributors)

## Project Structure

This is a **single-file** OCaml implementation that includes:
- **Abstract Syntax Tree (AST)**
- **Lexer**
- **Parser**
- **Type Checker**
- **Interpreter**
- **Test Cases**

The entire compiler is contained in **`compiler.ml`**.

## Syntax Overview

The language supports:
- **Integer and Boolean literals**
- **Binary operations**: `+`, `-`, `*`, `/`, `<=`, `>=`, `&&`, `||`
- **Conditional expressions**: `if ... then ... else ...`
- **Let bindings**: `let x = 10 in x + 5`
- **Functions**: `fun x -> x * 2`
- **Function application**: `(fun x -> x * 2) 5`
- **Lists**: `[]`, `1::2::[]`
- **Pattern matching**: `match lst with [] -> 0 | x::xs -> x`

## Example Expression
```ocaml
let f = fun x -> x * x in f 3
```
This defines a function `f` that squares a number and applies it to `3`, producing `9`.

## Example Test Cases
```ocaml
make_t "int" 22 "22";
make_t "add" 22 "11+11";
```

## Building and Running

### 1. Install OCaml and Dependencies
Make sure you have OCaml installed. If not, install it using:
```sh
opam install ocaml dune ounit2
```

### 2. Create a Dune Project
Ensure you have a `dune` build file. The following will compile the project:
```sh
dune build
```

### 3. Run the Compiler
Execute the compiler using:
```sh
dune exec ./compiler.exe
```

### 4. Run Tests
Run the test suite with:
```sh
dune runtest
```

## Future Improvements
- Extend the language with user-defined data types.
- Implement optimizations and a bytecode compiler.
- Add error reporting with better debugging messages.

## Contributors
Malachi Rosario

