# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
C1=/home/dustin/lib/java/jsdk.jar:/home/dustin/lib/java/DBCB.jar
C2=/home/dustin/lib/java/RHash.jar:/home/dustin/lib/java/cos.jar
CLASSPATH=$(C1):$(C2):/home/dustin/lib/java/postgresql.jar:.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java

CLASSES=PhotoServlet.class PhotoHelper.class PhotoUtil.class \
	PhotoImage.class Toker.class PhotoLogEntry.class \
	PhotoLogFlusher.class PhotoLogger.class PhotoLogView.class \
	PhotoLogImageEntry.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

test: $(CLASSES)
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

install: $(CLASSES)
	$(SCP) $(CLASSES) $(DEST)

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
