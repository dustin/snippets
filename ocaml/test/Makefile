# arch-tag: 3563BCD8-295B-11D8-8858-000393CB0F1E
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=b64test cdbtest cdbtestbc

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

b64test: b64test.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa b64test.ml

cdbtest: cdbtest.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		spy.cmxa cdbtest.ml

cdbtestbc: cdbtest.ml ../lib/extoption.cmo ../lib/cdb.cmo
	$(OCAMLC) $(PROFILE) -w A -g -o $@ -I ../lib \
		extoption.cmo cdb.cmo cdbtest.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
