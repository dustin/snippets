# Makefile for RHash, remote object server stuff.

JAR=/usr/pkg/java/bin/jar
JAVAC=/usr/pkg/java/bin/javac
RMIC=/usr/pkg/java/bin/rmic

CLASSES=Example.class RHash.class RObject.class RObjectImpl.class \
	RObjectImpl_Skel.class RObjectImpl_Stub.class

.SUFFIXES: .java .class .jar

all: RHash.jar

clean:
	rm -f $(CLASSES) RHash.jar

RHash.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

RObjectImpl_Skel.class: RObjectImpl.class
	$(RMIC) -d . RObjectImpl

.java.class:
	$(JAVAC) $<
