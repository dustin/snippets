# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/gifservlet.jar
C2=$(MYLIB)/spy.jar
CLASSPATH=$(C1):$(C2):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

CLASSES=net/spy/house/HouseServlet.class

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
