# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.3 2002/06/05 19:37:04 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-g -Wall -Werror
LDFLAGS=-lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
