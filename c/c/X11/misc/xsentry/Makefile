# Copyright (c) 2001  Dustin Sallings <dustin@spy.net>

DEFINES=-DDEBUG -DALARMLENGTHDEFAULT=5
CFLAGS=-g -Wall -pipe $(DEFINES) -I/usr/X11R6/include
LIBS=-L/usr/X11R6/lib -lX11
LDFLAGS=-g $(LIBS)
OBJS=X11.o main.o
SOURCES=X11.c main.c
OTHER=xsentry.h
EXES=xsentry
NAME=xsentry
JUNK=$(OBJS) $(EXES) *~

all: xsentry

xsentry: $(OBJS)
	cc -$(CFLAGS) -o $(NAME) $(OBJS) $(LDFLAGS)

tgz: $(SOURCES)
	@tar -czvf $(NAME).tgz Makefile $(SOURCES) $(OTHER)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
