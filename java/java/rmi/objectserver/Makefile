# Makefile for RHash, remote object server stuff.

# JAVAHOME=/usr/local/jdk1.1.8
JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
RMIC=$(JAVAHOME)/bin/rmic
JAVA=$(JAVAHOME)/bin/java

CLASSES=Example.class RHash.class RObject.class RObjectImpl.class \
	RObjectImpl_Skel.class RObjectImpl_Stub.class

.SUFFIXES: .java .class .jar

all: RHash.jar

clean:
	rm -f $(CLASSES) RHash.jar

RHash.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

serve: RHash.jar
	$(JAVA) -Djava.rmi.server.codebase=file://${PWD}/ RObjectImpl

RObjectImpl_Skel.class: RObjectImpl.class
	$(RMIC) -d . RObjectImpl

.java.class:
	$(JAVAC) $<
