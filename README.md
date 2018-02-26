# SQUALL Compiler

Project for the course "Web Data Management" given by Serge Abiteboul and Pierre
Senellart at MPRI, Paris Diderot.

This is an implementation of the controlled language to SPARQL compiler
described [here](https://hal.inria.fr/hal-00779946/).

## Requirements
Any decently recent version of OCaml should do (tested on `>4.04`).

The `csv`, `ppx_deriving`, `ocamlbuild` and `menhir` packages are also required, check
```
opam install ocamlbuild menhir ppx_deriving csv
```

## Usage
To compile the project, juste use `make`.
You may also need to clean the folder by removing all the files cleated by the compilation with `make clean`.

To start the program, you can either:
  - Enter interactive mode by executing `./squallc [-v]`. In this mode you can type squall queries which SPARQL in the terminal.
  - Use `./squallc [-v] file.txt`, where `file.txt` contains a single squall query on its first line. The result is then written in `file.rq`.

The `-v` option allows verbose mode, meaning that all intermediate forms of the query will be printed (either in the terminal or in the output file). Please note that there might be a lot of printing when using this option.

## Examples
You might find some examples of squall queries in the `test` folder.
