# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=/home/dustin/lib/java/swingall.jar:/home/dustin/lib/java/postgresql.jar
C2=/home/dustin/lib/java/spy.jar:$(MYLIB)/oromatcher.jar
CLASSPATH=$(C1):$(C2):.

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java

CLASSES=eBayWatch.class Watcher.class

.SUFFIXES: .java .class .jar

all: $(CLASSES)

run: $(CLASSES)
	env CLASSPATH=$(CLASSPATH) $(JAVA) eBayWatch

install: $(CLASSES)
	$(SCP) $(CLASSES) $(DEST)

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
