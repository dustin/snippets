# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.2 1997/04/15 06:11:41 dustin Exp $

.SUFFIXES: .m .o

CFLAGS=-I../include -g
AR=ar cr
RANLIB=ranlib
OBJC=gcc
LIBS=-lobjc
LDFLAGS=$(LIBS)
EXES=test
MISCOBJS=dString.o dSocket.o dWeb.o
SOURCE=dString.m dSocket.m dWeb.m
JUNK=$(EXES) *.o *.a

misc.a:	$(MISCOBJS)
	$(AR) $@ $(MISCOBJS)
	$(RANLIB) $@

test:   misc.a test.o
	$(OBJC) -o $@ test.o misc.a $(LIBS)

clean:
	-rm -f $(JUNK)

.m:	$@.o
	$(OBJC) -o $> $< $(LDFLAGS)

.o:
	$(OBJC) -o $> $< $(LDFLAGS)

.m.o:
	$(OBJC) $(CFLAGS) -c $<
