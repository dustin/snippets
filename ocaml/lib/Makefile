# arch-tag: 879F039C-05DC-11D8-B67C-000393DC8AE4
MKLIB=ocamlmklib
OCAMLMKTOP=ocamlmktop
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

PARTS=extstring fileutils lru linkedlist base64 cdb netstring extlist \
	extoption extstream

.SUFFIXES: .ml .mli .cmi .cmx

.PHONY: $(PARTS)

all: libspy docs

base64: base64.cmi base64.mli

fileutils: fileutils.cmi fileutils.mli

extstring: extstring.cmi extstring.mli

extlist: extlist.cmi extlist.mli

extoption: extoption.cmi extoption.mli

linkedlist: linkedlist.cmi linkedlist.mli

lru: linkedlist lru.cmi lru.mli

cdb: extoption cdb.cmi cdb.mli

netstring: netstring.cmi netstring.mli

extstream: extstream.cmi extstream.mli

testtop: $(PARTS)
	$(OCAMLMKTOP) -o testtop unix.cma \
		fileutils.cmo extstring.cmo extlist.cmo extoption.cmo \
		extstream.cmo linkedlist.cmo lru.cmo base64.cmo \
		cdb.cmo netstring.cmo

libspy: $(PARTS)
	$(MKLIB) -o spy \
		fileutils.cmo extstring.cmo extlist.cmo extoption.cmo \
		extstream.cmo linkedlist.cmo lru.cmo base64.cmo \
		cdb.cmo netstring.cmo \
		fileutils.cmx extstring.cmx extlist.cmx extoption.cmx \
		extstream.cmx linkedlist.cmx lru.cmx base64.cmx \
		cdb.cmx netstring.cmx
	ln spy.a libspy.a


docs: $(PARTS)
	mkdir -p doc
	ocamldoc -t "Dustin's OCaml Docs" -keep-code -colorize-code -d doc -html \
		*.mli *.ml

install-docs: docs
	cp doc/* /afs/spy.net/home/dustin/public_html/projects/ocaml/doc

clean:
	rm -rf testtop *.a *.cma *.cmxa *.cmx *.cmi *.cmo *.o doc

# .ml.mli:
	# $(OCAMLOPT) -i $< > $@

.mli.cmi: $<
	$(MKLIB) $< `echo $< | sed s/.mli/.ml/`
