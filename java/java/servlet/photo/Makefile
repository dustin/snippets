# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/DBCB.jar
C2=$(MYLIB)/RHash.jar:$(MYLIB)/cos.jar
C3=$(MYLIB)/spy.jar:$(MYLIB)/postgresql.jar:$(MYLIB)/xml4j.jar
C4=$(MYLIB)/ImageServer.jar
CLASSPATH=$(C1):$(C2):$(C3):$(C4):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=rcp
DEST=bleu.west.spy.net:/usr/local/apache/java
DEST=170.1.69.194:/usr/local/apache/java

CLASSES=net/spy/photo/PhotoServlet.class net/spy/photo/PhotoHelper.class \
	net/spy/photo/PhotoUtil.class net/spy/photo/PhotoImage.class \
	net/spy/photo/PhotoLogFlusher.class net/spy/photo/PhotoLogView.class \
	net/spy/photo/PhotoLogImageEntry.class net/spy/photo/PhotoUser.class \
	net/spy/photo/PhotoSecurity.class net/spy/photo/PhotoConfig.class \
	net/spy/photo/PhotoSearch.class net/spy/photo/PhotoStorerThread.class \
	net/spy/photo/PhotoSession.class

.SUFFIXES: .java .class .jar

all: photo.jar

photo.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

setpw: net/spy/photo/SetPW.class
	env CLASSPATH=$(CLASSPATH) $(JAVA) net.spy.photo.SetPW

install: all
	$(SCP) photo.jar $(DEST)

clean:
	rm -f $(CLASSES) photo.jar

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
