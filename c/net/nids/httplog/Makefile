# Generated automatically from Makefile.in by configure.
# Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
#
# $Id: Makefile,v 1.2 2001/02/25 11:03:37 dustin Exp $

DEFS= -DHAVE_LIBNET=1 -DHAVE_LIBPCAP=1 -DHAVE_LIBNIDS=1 -DSTDC_HEADERS=1 -DHAVE_LIMITS_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STRFTIME=1 -DHAVE_STRDUP=1 -DHAVE_STRSTR=1
CFLAGS= -DLIBNET_LIL_ENDIAN -I/usr/local/include $(DEFS) -DMYMALLOC -Wall -g
LDFLAGS=-g -L/usr/local/lib -lnids -lpcap -lnet

OBJS=httplog.o mymalloc.o

httplog: $(OBJS)
	$(CC) -o $@ httplog.o mymalloc.o $(LDFLAGS)

clean:
	rm -f httplog $(OBJS)

distclean: clean
	rm -f config.log config.cache config.status Makefile

httplog.o: httplog.c httplog.h
