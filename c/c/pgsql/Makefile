# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.2 2002/02/27 10:15:35 dustin Exp $

CFLAGS=-I/usr/local/pgsql/include
LDFLAGS=-L/usr/local/pgsql/lib -lpq

all: test tablecounter

test: test.o
	$(CC) -o $@ test.o $(LDFLAGS)

tablecounter: tablecounter.o
	$(CC) -o $@ tablecounter.o $(LDFLAGS)

clean:
	rm -f test test.o
