BIN=/emul/linux/usr/local/bin
PILRC=pilrc
CC=$(BIN)/m68k-palmos-coff-gcc
CFLAGS=-O1 #-DERROR_CHECK_LEVEL=2
LD=$(BIN)/m68k-palmos-coff-gcc
OBJRES=$(BIN)/m68k-palmos-coff-obj-res

OBJS=irtime.o

.c.o:
	$(CC) $(CFLAGS) -c $<

irtime.prc: timestamp-grc timestamp-bin
	$(BIN)/build-prc irtime.prc "IrTime" IRTM *.grc *.bin

timestamp-bin: irtime.rcp
	$(PILRC) irtime.rcp
	touch timestamp-bin

timestamp-grc: $(OBJS)
	$(CC) -O1 $(OBJS) -o irtime
	$(OBJRES) irtime
	touch timestamp-grc

irtime.c: irtime.h

irtime.rcp: irtime.h

clean:
	rm -f *~
	rm -f irtime $(OBJS) irtime.prc
	rm -f *.bin *.grc
	rm -f timestamp-grc timestamp-bin

distclean: clean
	rm -f irtime.prc
