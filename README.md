# SQUALL Compiler

Project for the course "Web Data Management" given by Serge Abiteboul and Pierre
Senellart at MPRI, Paris Diderot.

This is an implementation of the controlled language to SPARQL compiler
described [here](https://hal.inria.fr/hal-00779946/).

## Requirements
A fairly recent version of the OCaml compiler is required `>= 4.04`.

The `xml-light`, `ppx_deriving` and `menhir` packages are also required, check
```
opam install xml-light menhir ppx_deriving
```
