# Copyright (c) 1998  Dustin Sallings
# $Id: Makefile,v 1.1 2002/06/05 16:51:54 dustin Exp $
#
# Makefile for Parallel Do

SHELL=/bin/sh

CFLAGS=-g
LDFLAGS=-lz

logmerge: logmerge.o
	$(CC) -o $@ logmerge.o $(LDFLAGS)

clean:
	rm -f *.o logmerge
