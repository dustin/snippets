# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/gifservlet.jar
C2=$(MYLIB)/spy.jar
CLASSPATH=$(C1):$(C2):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=rcp
DEST=bleu.west.spy.net:/usr/local/apache/java/net/spy/temperature/
# DEST=170.1.69.194:/usr/local/apache/java

CLASSES=net/spy/chart/TwoPointGraph.class \
		net/spy/chart/BarGraph.class \
		net/spy/chart/BarGraphServlet.class

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
