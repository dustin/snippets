# arch-tag: 879F039C-05DC-11D8-B67C-000393DC8AE4
MKLIB=ocamlmklib
OCAMLMKTOP=ocamlmktop
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

PARTS=stringutils fileutils lru linkedlist base64 cdb

.SUFFIXES: .ml .mli .cmi .cmx

.PHONY: $(PARTS)

all: $(PARTS) docs

base64: base64.cmi base64.mli

fileutils: fileutils.cmi fileutils.mli

stringutils: stringutils.cmi stringutils.mli

linkedlist: linkedlist.cmi linkedlist.mli

lru: linkedlist lru.cmi lru.mli

cdb: cdb.cmi cdb.mli

testtop: $(PARTS)
	$(OCAMLMKTOP) -o testtop unix.cma \
		fileutils.cmo stringutils.cmo linkedlist.cmo lru.cmo base64.cmo cdb.cmo

docs: $(PARTS)
	mkdir -p doc
	ocamldoc -d doc -html *.ml

clean:
	rm -rf testtop *.a *.cma *.cmxa *.cmx *.cmi *.cmo *.o doc

.ml.mli:
	$(OCAMLOPT) -i $< > $@

.mli.cmi: $<
	$(MKLIB) $< `echo $< | sed s/.mli/.ml/`
