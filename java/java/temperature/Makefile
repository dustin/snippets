# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
C1=/home/dustin/lib/java/postgresql.jar
CLASSPATH=$(C1):.

CLASSES=net/spy/temperature/Loader.class net/spy/temperature/TempSet.class

FDEST=/home/dustin/lib/java

.SUFFIXES: .java .class .jar

all: $(CLASSES)

install: all

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
