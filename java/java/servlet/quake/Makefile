# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
RMIC=$(JAVAHOME)/bin/rmic
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/spy.jar
CLASSPATH=$(C1):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

CLASSES=net/spy/quake/QuakeServlet.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

jar: $(CLASSES)
	$(JAR) cv0f quake.jar $(CLASSES)

test: $(CLASSES)
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

clean:
	rm -f `find net/spy -name "*.class"`

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
