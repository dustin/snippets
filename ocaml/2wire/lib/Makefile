# arch-tag: 879F039C-05DC-11D8-B67C-000393DC8AE4
MKLIB=ocamlmklib
OCAMLMKTOP=ocamlmktop
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

PARTS=stringutils fileutils lru

.SUFFIXES: .ml .mli .cmi .cmx

.PHONY: $(PARTS)

all: $(PARTS) docs

fileutils: fileutils.cmi fileutils.mli

stringutils: stringutils.cmi stringutils.mli

lru: lru.cmi lru.mli

testtop: $(PARTS)
	$(OCAMLMKTOP) -o testtop unix.cma \
		fileutils.cmo stringutils.cmo lru.cmo

docs:
	mkdir -p doc
	ocamldoc -d doc -html *.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o

.ml.mli:
	$(OCAMLOPT) -i $< > $@

.ml.cmi:
	$(MKLIB) $<
