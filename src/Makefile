OCAMLMAKEFILE = OCamlMakefile

SOURCES = remodel_ast.ml remodel_depgraph.ml remodel_cache.ml \
	remodel_parse.mly remodel_lex.mll remodel.ml
RESULT = remodel
PACKS = extlib unix parmap
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g
YFLAGS = -v

all: nc
include $(OCAMLMAKEFILE)

