# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.4 2002/06/06 01:03:55 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-funroll-loops -O2 -Wall -Werror
LDFLAGS=-lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
