# $Id: Makefile,v 1.1 1997/08/26 06:00:48 dustin Exp $

MAJOR=0
MINOR=1
PATCH=1
VERSION=$(MAJOR).$(MINOR).$(PATCH)
DEFINES= -DDEBUG=1 -DVERSION="$(VERSION)"
CFLAGS=-O2 $(DEFINES)

LIBS=
# Solaris
# LIBS=-lnsl -lsocket

SERVOBJS=sockets.o

SOURCE=sockets.c
OTHER=pageserv.h Makefile
NAME=pageserv

all: pageserv

pageserv: $(SERVOBJS)
	cc -o pageserv $(SERVOBJS) $(LIBS)

clean:
	rm -f core pageserv $(SERVOBJS)

dist:
	tar cvf $(NAME).tar $(SOURCE) $(OTHER)
	gzip -9v $(NAME).tar

# Dependencys
