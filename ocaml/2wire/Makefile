# arch-tag: E11ED3B2-194A-11D8-98E0-000393CFE6B8
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=
SPYINC=-I ../spy

PROGS= extractKeys validateNoDups validateCdb validateKeys validateDump \
	buildMfgDB mfgmerge cdbpick cdbcat cdbget validateKeysRev mfglookup

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

install: $(PROGS)
	cp $(PROGS) $(INSTDIR)

extractKeys: extractKeys.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx extractKeys.ml

validateCdb: validateCdb.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx validateCdb.ml

validateDump: validateDump.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx validateDump.ml

validateNoDups: validateNoDups.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx validateNoDups.ml

validateKeys: validateKeys.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx validateKeys.ml

validateKeysRev: validateKeysRev.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx validateKeysRev.ml

mfglookup: mfglookup.ml mfgcdb.cmi
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.cmx mfglookup.ml

cdbget: cdbget.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa cdbget.ml

buildMfgDB: buildMfgDB.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa buildMfgDB.ml

mfgmerge: mfgmerge.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgmerge.ml

cdbcat: cdbcat.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa cdbcat.ml

cdbpick: cdbpick.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa cdbpick.ml

mfgcdb: mfgcdb.ml
	$(OCAMLOPT) $(PROFILE) -o $@ $(SPYINC) \
		unix.cmxa spy.cmxa mfgcdb.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $(SPYINC) $<
