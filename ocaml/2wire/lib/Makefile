MKLIB=ocamlmklib
OCAMLOPT=ocamlopt

.SUFFIXES: .ml .mli .cmi .cmx

.PHONY: stringutils

all: stringutils

stringutils: stringutils.cmi stringutils.mli

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
