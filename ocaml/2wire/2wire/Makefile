# arch-tag: E11ED3B2-194A-11D8-98E0-000393CFE6B8
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=parseSQLLog parseTimingLog rrdIndex indexGWAuth spreadsheetPivot \
	mfgcdb mfgcdb.cmi extractAuthInfo snmap statutils statutils.cmi statRollup \
	rrdAggregate extractKeys buildMfgCache cdbprefix

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

extractKeys: extractKeys.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa mfgcdb.cmx extractKeys.ml

cdbprefix: cdbprefix.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa cdbprefix.ml

buildMfgCache: buildMfgCache.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa buildMfgCache.ml

buildMfgCachebc: buildMfgCache.ml
	$(OCAMLC) $(PROFILE) -w A -g -o $@ -I ../lib \
		unix.cma spy.cma buildMfgCache.ml

rrdIndex: rrdIndex.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa rrdIndex.ml

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

statutils: statutils.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa statutils.ml

rrdAggregate: rrdAggregate.ml statutils.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa statutils.cmx rrdAggregate.ml

statRollup: statRollup.ml statutils.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa statutils.cmx statRollup.ml

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
