# arch-tag: 7D42EFA7-05DC-11D8-AA29-000393DC8AE4
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=mailCompress hlgrep missingNumbers magicSquare

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

mailCompress: mailCompress.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib -I /usr/local/lib/ocaml/site-lib \
		unix.cmxa \
		stringutils.cmx fileutils.cmx linkedlist.cmx lru.cmx gz.cmxa \
		mailCompress.ml

hlgrep: hlgrep.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa str.cmxa fileutils.cmx hlgrep.ml

missingNumbers: missingNumbers.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		missingNumbers.ml

magicSquare: magicSquare.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		magicSquare.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
