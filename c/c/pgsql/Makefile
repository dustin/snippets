# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.4 2002/02/28 05:50:38 dustin Exp $

POSTGRES=/usr/local/pgsql

CFLAGS=-I$(POSTGRES)/include
LDFLAGS=-L$(POSTGRES)/lib -lpq

all: test tablecounter

test: test.o
	$(CC) -o $@ test.o $(LDFLAGS)

tablecounter: tablecounter.o
	$(CC) -o $@ tablecounter.o $(LDFLAGS)

clean:
	rm -f test test.o
