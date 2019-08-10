SOURCES = source.ml parser.mly lexer.mll target.ml gensym.ml cps.ml main.ml
RESULT = cps-calc
OCAMLMAKEFILE =  ~/include/OCamlMakefile
include $(OCAMLMAKEFILE)
