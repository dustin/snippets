# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.2 2002/06/05 19:32:16 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-g -Wall -Werror -DMYMALLOC=1
LDFLAGS=-lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
