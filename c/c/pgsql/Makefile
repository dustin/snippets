# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.3 2002/02/27 10:23:10 dustin Exp $

CFLAGS=-I/usr/pkg/pgsql/include
LDFLAGS=-L/usr/pkg/pgsql/lib -lpq -lcrypt

all: test tablecounter

test: test.o
	$(CC) -o $@ test.o $(LDFLAGS)

tablecounter: tablecounter.o
	$(CC) -o $@ tablecounter.o $(LDFLAGS)

clean:
	rm -f test test.o
