# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAVAC=jikes +P
JAVA=$(JAVAHOME)/bin/java
RMIC=$(JAVAHOME)/bin/rmic
MYLIB=$(HOME)/lib/java
S=/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar
C1=$S:$(MYLIB)/jsdk.jar:$(MYLIB)/xmlrpc.jar:$(MYLIB)/spy.jar
CLASSPATH=$(C1):.

CLASSES=\
	net/spy/rpc/XMLRPC.class \
	net/spy/rpc/services/ZipCode.class \
	net/spy/rpc/services/Temperatures.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
