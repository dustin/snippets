# arch-tag: E11ED3B2-194A-11D8-98E0-000393CFE6B8
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=parseSQLLog parseTimingLog rrdIndex indexGWAuth

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

parseSQLLog: parseSQLLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx fileutils.cmx parseSQLLog.ml

parseTimingLog: parseTimingLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx fileutils.cmx parseTimingLog.ml

parseTimingLogbc: parseTimingLog.ml
	$(OCAMLC) $(PROFILE) -w A -g -o $@ -I ../lib \
		unix.cma stringutils.cmo fileutils.cmo parseTimingLog.ml

rrdIndex: rrdIndex.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa dbm.cmxa stringutils.cmx fileutils.cmx rrdIndex.ml

indexGWAuth: indexGWAuth.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa dbm.cmxa stringutils.cmx fileutils.cmx indexGWAuth.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
