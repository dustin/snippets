# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.6 2002/06/06 05:49:32 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-O2 -Wall -Werror # -pg -g -DUSE_ASSERT
LDFLAGS=-O2 -lz

logmerge: logmerge.o mymalloc.o
	$(CC) -o $@ logmerge.o mymalloc.o $(LDFLAGS)

logmerge.o: logmerge.c logmerge.h

clean:
	rm -f *.o logmerge core
