# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.5 2002/06/06 04:01:24 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-g -Wall -Werror
LDFLAGS=-g -lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
