# Copyright (c) 1997  Dustin Sallings
# $Id: Makefile,v 1.1 1997/10/01 07:06:28 dustin Exp $

CFLAGS=-I/usr/local/pgsql/include
LDFLAGS=-L/usr/local/pgsql/lib -lpq

all: test

test: test.o
	$(CC) -o $@ test.o $(LDFLAGS)

clean:
	rm -f test test.o
