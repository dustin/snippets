# arch-tag: 7D42EFA7-05DC-11D8-AA29-000393DC8AE4
MKLIB=ocamlmklib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

# Set this to -p for profiler support
PROFILE=

PROGS=mailCompress hlgrep missingNumbers magicSquare waitForSocket wx200 \
	builddict cipher splitFreqs lncp resolver ager blockDigest blockDigest.cmi \
	digestline cdbcat cdbpick cdbget cdbtest

.SUFFIXES: .ml .mli .cmi .cmx

all: $(PROGS)

mailCompress: mailCompress.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib -I /usr/local/lib/ocaml/site-lib \
		unix.cmxa \
		spy.cmxa lru.cmx gz.cmxa \
		mailCompress.ml

hlgrep: hlgrep.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa str.cmxa fileutils.cmx hlgrep.ml

cdbtest: cdbtest.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa cdbtest.ml

cdbget: cdbget.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa cdbget.ml

cdbcat: cdbcat.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa cdbcat.ml

cdbpick: cdbpick.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa cdbpick.ml

digestline: digestline.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa fileutils.cmx digestline.ml

missingNumbers: missingNumbers.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		missingNumbers.ml

magicSquare: magicSquare.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		magicSquare.ml

waitForSocket: waitForSocket.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib unix.cmxa \
		waitForSocket.ml

largefile: largefile.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib unix.cmxa \
		largefile.ml

resolver: resolver.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib unix.cmxa spy.cmxa \
		resolver.ml

ager: ager.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib unix.cmxa spy.cmxa \
		ager.ml

wx200: wx200.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib unix.cmxa \
		wx200.ml

builddict: builddict.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa builddict.ml

cipher: cipher.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa str.cmxa spy.cmxa cipher.ml

makeCipher: makeCipher.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa makeCipher.ml

splitFreqs: splitFreqs.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa splitFreqs.ml

blockDigest: blockDigest.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa blockDigest.ml

lncp: lncp.ml
	$(OCAMLOPT) $(PROFILE) -o $@ -I ../lib \
		unix.cmxa spy.cmxa lncp.ml

clean:
	rm -f *.a *.cma *.cmxa *.cmx *.mli *.cmi *.cmo *.o
	rm -f $(PROGS)

.ml.mli:
	$(OCAMLOPT) -o .buildtmp -i $< > $@
	rm .buildtmp

.ml.cmi:
	$(MKLIB) $<
