# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
C1=/home/dustin/lib/java/jsdk.jar:/home/dustin/lib/java/DBCB.jar
C2=/home/dustin/lib/java/RHash.jar:/home/dustin/lib/java/cos.jar
CLASSPATH=$(C1):$(C2):/home/dustin/lib/java/postgresql.jar:.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java
# DEST=170.1.69.194:/usr/local/apache/java

CLASSES=PhotoServlet.class PhotoHelper.class PhotoUtil.class \
	PhotoImage.class Toker.class PhotoLogEntry.class \
	PhotoLogFlusher.class PhotoLogger.class PhotoLogView.class \
	PhotoLogImageEntry.class PhotoUser.class PhotoImageData.class \
	PhotoSecurity.class PhotoConfig.class

.SUFFIXES: .java .class .jar

all: photo.jar

photo.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

setpw: SetPW.class
	env CLASSPATH=$(CLASSPATH) $(JAVA) SetPW

install: all
	$(SCP) PhotoServlet.class photo.jar $(DEST)

clean:
	rm -f $(CLASSES) photo.jar

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
