# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
RMIC=$(JAVAHOME)/bin/rmic
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/spy.jar
C2=$(MYLIB)/resin-xsl.jar:$(MYLIB)/sax.jar:$(MYLIB)/dom.jar
CLASSPATH=$(C1):$(C2):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

CLASSES=net/spy/rss/RSSServlet.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

jar: $(CLASSES)
	$(JAR) cv0f rss.jar $(CLASSES)

test: $(CLASSES)
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

clean:
	rm -f `find net/spy -name "*.class"`

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
