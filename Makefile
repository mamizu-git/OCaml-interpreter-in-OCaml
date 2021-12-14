SOURCES = lexer.mll parser.mly tySyntax.mli tySyntax.ml constraintSolver.mli constraintSolver.ml syntax.ml eval.ml main.ml
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile

