# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.1 1997/04/15 06:09:16 dustin Exp $

.SUFFIXES: .m .o

CFLAGS=-I../include -g
AR=ar cr
RANLIB=ranlib
OBJC=gcc
LIBS=../misc/misc.a -lobjc
LDFLAGS=$(LIBS)
EXES=httpd
HTTPD_OBJS=httpd.o
SOURCE=httpd.m
JUNK=$(EXES) *.o *.a

all:	$(EXES)

httpd:	$(HTTPD_OBJS)
	$(OBJC) -o $@ $(HTTPD_OBJS) $(LDFLAGS)

clean:
	-rm -f $(JUNK)

.m:	$@.o
	$(OBJC) -o $> $< $(LDFLAGS)

.o:
	$(OBJC) -o $> $< $(LDFLAGS)

.m.o:
	$(OBJC) $(CFLAGS) -c $<
