# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac -deprecation
JAVA=$(JAVAHOME)/bin/java
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/spy.jar
CLASSPATH=$(C1):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

CLASSES=net/spy/debugservlet/Debug.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

install: all
	$(SCP) $(CLASSES) $(DEST)

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
