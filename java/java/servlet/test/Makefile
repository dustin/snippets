# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/pkg/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/DBCB.jar
C2=$(MYLIB)/RHash.jar:$(MYLIB)/cos.jar
C3=$(MYLIB)/spy.jar:$(MYLIB)/postgresql.jar:$(MYLIB)/xml4j.jar
CLASSPATH=$(C1):$(C2):$(C3):.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java
# DEST=170.1.69.194:/usr/local/apache/java

CLASSES=TestTaker.class net/spy/test/TestSession.class \
	net/spy/test/TestConfig.class net/spy/test/TestSessionSweeper.class \
	net/spy/test/TestUser.class

.SUFFIXES: .java .class .jar

all: testtaker.jar

testtaker.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

install: all
	# $(SCP) PhotoServlet.class photo.jar $(DEST)

clean:
	rm -f $(CLASSES) testtaker.jar

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
