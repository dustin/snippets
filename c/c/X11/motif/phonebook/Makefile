# Copyright (c) 1997  Dustin Sallings
#
# $Id: Makefile,v 1.3 1997/06/16 23:57:23 dustin Exp $

SHELL=/bin/sh
LBIN=/usr/local/bin
CDEBUGFLAGS=  -O2

LIBS=-lXm -lXt -lX11
LDFLAGS= $(LIBS)

CC=cc
RANLIB=:

CFLAGS=$(CDEBUGFLAGS)
PBKOBJS=phonebook.o callbacks.o widgets.o fields.o find.o
SRC=phonebook.c callbacks.c widgets.c find.c
EXES=phonebook
JUNK=fields.c fields.h core

all:	$(EXES)

phonebook: $(PBKOBJS)
	$(CC) -o $@ $(PBKOBJS) $(LDFLAGS)

depend:
	$(CC) -M *.c >> Makefile

fields.h: fieldlist makefields
	./makefields

fields.c: fieldlist makefields
	./makefields

# These have to be touched so they'll stop being built every time

callbacks.c: fields.h
	@touch $@

phonebook.c: fields.h
	@touch $@

find.c: fields.h
	@touch $@

install:	all man
	@echo Maybe later

clean:  splathack
	rm -f *.o *.a $(EXES) $(JUNK)

ultrabrite: clean
	rm -f $(AUTOJUNK)

splathack:
	@touch globbingsucks.o globbingsucks.a

