# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar
C2=$(HOME)/prog/java/servlet/png:$(MYLIB)/spy.jar
CLASSPATH=$(C1):$(C2):.

CLASSES=net/spy/house/HouseServlet.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

install: all
	$(SCP) $(CLASSES) $(DEST)

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
