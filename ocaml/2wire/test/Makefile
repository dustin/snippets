# arch-tag: 3563BCD8-295B-11D8-8858-000393CB0F1E
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=b64test

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

b64test: b64test.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx fileutils.cmx base64.cmx b64test.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
