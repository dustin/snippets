# Makefile for RHash, remote object server stuff.

# JAVAHOME=/usr/local/jdk1.1.8
JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
RMIC=$(JAVAHOME)/bin/rmic
JAVA=$(JAVAHOME)/bin/java

CLASSES=Example.class net/spy/ImageServer.class net/spy/ImageServerImpl.class \
	net/spy/ImageServerImpl_Skel.class net/spy/ImageServerImpl_Stub.class

.SUFFIXES: .java .class .jar

all: ImageServer.jar

clean:
	rm -f $(CLASSES) ImageServer.jar

ImageServer.jar: $(CLASSES)
	$(JAR) cv0f $@ net

serve: ImageServer.jar
	$(JAVA) -Djava.rmi.server.codebase=file://${PWD}/ net.spy.ImageServerImpl &

net/spy/ImageServerImpl_Skel.class: net/spy/ImageServerImpl.class
	$(RMIC) -d . net.spy.ImageServerImpl

net/spy/ImageServerImpl_Stub.class: net/spy/ImageServerImpl_Skel.class


.java.class:
	$(JAVAC) $<
