# Makefile for RHash, remote object server stuff.

# JAVAHOME=/usr/local/jdk1.1.8
JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
RMIC=$(JAVAHOME)/bin/rmic
JAVA=$(JAVAHOME)/bin/java

CLASSES=Example.class net/spy/RHash.class net/spy/RObject.class \
	net/spy/RObjectImpl.class net/spy/RObjectImpl_Skel.class \
	net/spy/RObjectImpl_Stub.class

.SUFFIXES: .java .class .jar

all: RHash.jar

clean:
	rm -f $(CLASSES) RHash.jar

RHash.jar: $(CLASSES)
	$(JAR) cv0f $@ net

serve: RHash.jar
	$(JAVA) -Djava.rmi.server.codebase=file://${PWD}/ net.spy.RObjectImpl &

net/spy/RObjectImpl_Skel.class: net/spy/RObjectImpl.class
	$(RMIC) -d . net.spy.RObjectImpl

.java.class:
	$(JAVAC) $<
