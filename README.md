This repository contains a toy static analysis framework, designed to be used
for [ENSIIE](https://www.ensiie.fr)'s course on static analysis. It is
written in OCaml and requires the following opam packages to be compiled:

- zarith
- ocamlgraph
- menhir
- dune

To compile it, just do `dune build live_var.exe` in this directory.

# The language

The language analyzed here is meant to be simple: it manipulates
unbounded integers, with only `+`, `*`, and `/` arithmetic operators.
Boolean expressions can be
defined using arithmetic comparison and basic boolean operators (conjunction,
disjunction, negation). Finally, you can take the address of a variable (`&x`)
and dereference a variable (`*y`, provided the value stored in `y` is actually
an address). There's no pointer arithmetic.

In order to evaluate an expression, we need to have
an environment, associating a value (either a numerical value or an address)
to some variables.
If the expression contains a memory location that is not present in the
environment, this memory location is evaluated to `0`.

Finally, instructions include assignment of an arithmetic expression to a
memory location, sequence, conditional and `while` loop.

# Sources

Sources are composed of the following files.

- `ast.ml` contains the definition of types representing the syntax of the
  language.
- `print.ml` defines a pretty-printer for outputting a program in textual form.
- `parser.mly` and `lexer.mll` take care of creating an abstract syntax tree
  from the text representation of a program
- `stan.ml` defines a generic functor to perform static analysis over a
  program
- `live_var.ml` proposes an example of static analysis
- `constant_propagation.ml` and `sign.ml` are two exercises to be completed.
