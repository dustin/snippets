# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.7 2002/06/06 18:35:12 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-O2 -Wall -Werror -g -DUSE_ASSERT
LDFLAGS=-O2 -g -lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
