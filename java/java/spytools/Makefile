# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
C1=/home/dustin/lib/java/jsdk.jar:/home/dustin/lib/java/DBCB.jar
C2=/home/dustin/lib/java/RHash.jar:/home/dustin/lib/java/cos.jar
C3=/home/dustin/lib/java/xml4j.jar
CLASSPATH=$(C1):$(C2):$(C3):/home/dustin/lib/java/postgresql.jar:.

CLASSES=net/spy/SpyConfig.class net/spy/SpyDB.class \
	net/spy/SpyLog.class net/spy/SpyLogEntry.class \
	net/spy/SpyToker.class net/spy/SpyLogFlusher.class \
	net/spy/SpyConfigReader.class net/spy/net/SNPP.class \
	net/spy/net/HTTPFetch.class net/spy/SpyUtil.class \
	net/spy/util/ImageData.class

FDEST=/home/dustin/lib/java
SCP=rcp
SDEST=bleu.west.spy.net:/usr/local/apache/java

.SUFFIXES: .java .class .jar

all: spy.jar

spy.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

install: all
	cp spy.jar $(FDEST)
	$(SCP) spy.jar $(SDEST)

clean:
	rm -f $(CLASSES) spy.jar

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
