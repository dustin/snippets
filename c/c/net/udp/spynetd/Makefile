# $Id: Makefile,v 1.1 1998/01/15 09:17:57 dustin Exp $
DEFINES= #-DDEBUG
CFLAGS=-s -Wall -m486 -O6 -pipe $(DEFINES) -L/home/dustin/lib/msql/lib \
-I/home/dustin/lib/msql/include
MSQL=-lmsql
LIBS=
LDFLAGS=-s $(LIBS)
OBJS=spynetd.o bigbro.o
SOURCES=spynetd.c bigbro.c
OTHER=protocol.h
EXES=bigbro spynetd
NAME=spynetd
JUNK=$(OBJS) $(EXES) *.core

all: spynetd bigbro

spynetd: spynetd.o
	cc $(CFLAGS) -o spynetd spynetd.o $(LDFLAGS)

bigbro: bigbro.o
	cc $(CFLAGS) -o bigbro bigbro.o $(LDFLAGS) $(MSQL)

bigbro.o: bigbro.c protocol.h
#	ci -mcompile -l bigbro.c
	cc $(CFLAGS) -c bigbro.c

spynetd.o: spynetd.c protocol.h
#	ci -mcompile -l spynetd.c
	cc $(CFLAGS) -c spynetd.c

tgz:
	@tar -czvf $(NAME).tgz Makefile $(SOURCES) $(OTHER)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
