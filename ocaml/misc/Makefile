# arch-tag: 7D42EFA7-05DC-11D8-AA29-000393DC8AE4
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=parseSQLLog parseTimingLog mailCompress

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

parseSQLLog: parseSQLLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx fileutils.cmx parseSQLLog.ml

parseTimingLog: parseTimingLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa stringutils.cmx fileutils.cmx parseTimingLog.ml

mailCompress: mailCompress.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib -I /usr/local/lib/ocaml/site-lib \
		unix.cmxa \
		stringutils.cmx fileutils.cmx gz.cmxa \
		mailCompress.ml

parseTimingLogbc: parseTimingLog.ml
	$(OCAMLC) $(PROFILE) -w A -g -o $@ -I ../lib \
		unix.cma stringutils.cmo fileutils.cmo parseTimingLog.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
