# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
C1=/home/dustin/lib/java/jsdk.jar:/home/dustin/lib/java/DBCB.jar
C2=/home/dustin/lib/java/RHash.jar:/home/dustin/lib/java/cos.jar
C3=/home/dustin/lib/java/spy.jar:/home/dustin/lib/java/postgresql.jar
CLASSPATH=$(C1):$(C2):$(C3):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java
# DEST=170.1.69.194:/usr/local/apache/java

CLASSES=PhotoServlet.class net/spy/photo/PhotoHelper.class \
	net/spy/photo/PhotoUtil.class \
	net/spy/photo/PhotoImage.class \
	net/spy/photo/PhotoLogFlusher.class net/spy/photo/PhotoLogView.class \
	net/spy/photo/PhotoLogImageEntry.class net/spy/photo/PhotoUser.class \
	net/spy/photo/PhotoImageData.class \
	net/spy/photo/PhotoSecurity.class net/spy/photo/PhotoConfig.class \
	net/spy/photo/PhotoSearch.class \
	net/spy/photo/PhotoStorerThread.class net/spy/photo/PhotoSession.class

.SUFFIXES: .java .class .jar

all: photo.jar

photo.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

setpw: net/spy/photo/SetPW.class
	env CLASSPATH=$(CLASSPATH) $(JAVA) net.spy.photo.SetPW

install: all
	$(SCP) PhotoServlet.class photo.jar $(DEST)

clean:
	rm -f $(CLASSES) photo.jar

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
