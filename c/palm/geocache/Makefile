BIN=/emul/linux/usr/local/bin
PILRC=pilrc
CC=$(BIN)/m68k-palmos-coff-gcc
CFLAGS=-O1 #-DERROR_CHECK_LEVEL=2
LD=$(BIN)/m68k-palmos-coff-gcc
OBJRES=$(BIN)/m68k-palmos-coff-obj-res

OBJS=geocache.o

.c.o:
	$(CC) $(CFLAGS) -c $<

geocache.prc: timestamp-grc timestamp-bin
	$(BIN)/build-prc geocache.prc "GeoCache" GEOC *.grc *.bin

timestamp-bin: geocache.rcp
	$(PILRC) geocache.rcp
	touch timestamp-bin

timestamp-grc: $(OBJS)
	$(CC) -O1 $(OBJS) -o geocache
	$(OBJRES) geocache
	touch timestamp-grc

geocache.c: geocache.h

geocache.rcp: geocache.h

clean:
	rm -f *~
	rm -f geocache $(OBJS) geocache.prc
	rm -f *.bin *.grc
	rm -f timestamp-grc timestamp-bin

distclean: clean
	rm -f geocache.prc
