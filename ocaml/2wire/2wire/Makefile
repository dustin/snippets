# arch-tag: E11ED3B2-194A-11D8-98E0-000393CFE6B8
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=parseSQLLog parseTimingLog rrdIndex indexGWAuth spreadsheetPivot \
	mfgcdb mfgcdb.cmi extractAuthInfo snmap

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

parseSQLLog: parseSQLLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa parseSQLLog.ml

parseTimingLog: parseTimingLog.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa parseTimingLog.ml

parseTimingLogbc: parseTimingLog.ml
	$(OCAMLC) $(PROFILE) -w A -g -o $@ -I ../lib \
		unix.cma spy.cma parseTimingLog.ml

rrdIndex: rrdIndex.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa dbm.cmxa spy.cmxa rrdIndex.ml

spreadsheetPivot: spreadsheetPivot.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa spreadsheetPivot.ml

indexGWAuth: indexGWAuth.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa dbm.cmxa spy.cmxa indexGWAuth.ml

mfgcdb: mfgcdb.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa mfgcdb.ml

snmap: snmap.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa snmap.ml

extractAuthInfo: extractAuthInfo.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa mfgcdb.cmx extractAuthInfo.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) -I ../lib $<
