# Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
#
# $Id: Makefile,v 1.1 2000/10/18 09:09:32 dustin Exp $

SYSTEM=netbsd
JAVAHOME=/usr/pkg/java
JBIN=$(JAVAHOME)/bin
JAVAC=$(JBIN)/javac
JAVAH=$(JBIN)/javah
JAVA=$(JBIN)/java

# For compiling native code
CFLAGS=-O2 -fPIC -I$(JAVAHOME)/include -I$(JAVAHOME)/include/$(SYSTEM)

.SUFFIXES: .java .class .c .o .so

# The so can't be built until everything else is, so we'll make that our
# default target.
all: libhello.so

# Run is, because we're lazy
test: all
	env LD_LIBRARY_PATH=. $(JAVA) HelloWorld

# Get rid of crap that gets generated
clean:
	rm -f libhello.so HelloWorld.class HelloWorldImpl.o HelloWorld.h

# This so requires the c file to be compiled, of course
libhello.so: HelloWorldImpl.o
	$(CC) -shared -o $@ HelloWorldImpl.o

# This file is generated from the class
HelloWorld.h: HelloWorld.class
	$(JAVAH) HelloWorld

# This file requires the header file to be generated
HelloWorldImpl.o: HelloWorld.h

# How to turn a .java into a .class
.java.class: $<
	$(JAVAC) $<
