# Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
#
# $Id: Makefile,v 1.1 2000/10/18 09:53:13 dustin Exp $

SYSTEM=netbsd
JAVAHOME=/usr/pkg/java
JBIN=$(JAVAHOME)/bin
JAVAC=$(JBIN)/javac
JAVAH=$(JBIN)/javah
JAVA=$(JBIN)/java

# For compiling native code
CFLAGS=-O2 -fPIC -I$(JAVAHOME)/include -I$(JAVAHOME)/include/$(SYSTEM) \
	-I$(PWD)

.SUFFIXES: .java .class .c .o .so

CLASSES=net/spy/jni/RawInput.class

# The so can't be built until everything else is, so we'll make that our
# default target.
all: native/librawinput.so

# Run is, because we're lazy
test: all
	env LD_LIBRARY_PATH=native $(JAVA) net.spy.jni.RawInput

# Get rid of crap that gets generated
clean:
	rm -f native/librawinput.so $(CLASSES) \
		native/RawInputImpl.o net_spy_jni_RawInput.h

# This so requires the c file to be compiled, of course
native/librawinput.so: native/RawInputImpl.o
	$(CC) -shared -o $@ native/RawInputImpl.o

# This file is generated from the class
net_spy_jni_RawInput.h: net/spy/jni/RawInput.class
	$(JAVAH) net.spy.jni.RawInput

# This file requires the header file to be generated
native/RawInputImpl.o: net_spy_jni_RawInput.h

# How to turn a .c into a .o (the default target was wrong)
.c.o: $<
	$(CC) $(CFLAGS) -c -o $@ $<

# How to turn a .java into a .class
.java.class: $<
	$(JAVAC) $<
