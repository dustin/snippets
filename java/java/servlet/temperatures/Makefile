# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac -deprecation
JAVA=$(JAVAHOME)/bin/java
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/gifservlet.jar
C2=$(MYLIB)/spy.jar
CLASSPATH=$(C1):$(C2):.

CLASSES=net/spy/temperature/Temperature.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

install: all
	$(SCP) $(CLASSES) $(DEST)

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
