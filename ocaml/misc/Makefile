MKLIB=ocamlmklib
OCAMLOPT=ocamlopt

# Set this to -p for profiler support
PROFILE=

PROGS=parseSQLLog parseTimingLog

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

parseSQLLog: parseSQLLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx parseSQLLog.ml

parseTimingLog: parseTimingLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx parseTimingLog.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<