# Copyright (c) 1997  Dustin Sallings
#
# $Id: Makefile,v 1.1 1997/06/16 13:46:05 dustin Exp $

SHELL=/bin/sh
LBIN=/usr/local/bin
CDEBUGFLAGS=  -g

LIBS=-lXm -lXt -lX11
LDFLAGS= $(LIBS)

CC=cc
RANLIB=:

CFLAGS=$(CDEBUGFLAGS)
PBKOBJS=phonebook.o callbacks.o widgets.o fields.o
SRC=phonebook.c callbacks.c widgets.c
EXES=phonebook
JUNK=fields.c fields.h

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

install:	all man
	@echo Maybe later

clean:  splathack
	rm -f *.o *.a $(EXES) $(JUNK)

ultrabrite: clean
	rm -f $(AUTOJUNK)

splathack:
	@touch globbingsucks.o globbingsucks.a

