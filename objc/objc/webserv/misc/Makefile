# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.1 1997/04/14 22:12:47 dustin Exp $

.SUFFIXES: .m .o

CFLAGS=-I../include -g -Wall
OBJC=gcc
LIBS=-lobjc
LDFLAGS=$(LIBS)
EXES=test
JUNK=$(EXES) *.o

test:   dString.o test.o
	$(OBJC) -o $@ dString.o test.o $(LIBS)

clean:
	-rm -f $(JUNK)

.m:	$@.o
	$(OBJC) -o $> $< $(LDFLAGS)

.o:
	$(OBJC) -o $> $< $(LDFLAGS)

.m.o:
	$(OBJC) $(CFLAGS) -c $<
