# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=/home/dustin/lib/java
C1=$(MYLIB)/DBCB.jar:$(MYLIB)/jndi.jar:$(MYLIB)/rmiregistry.jar
C2=$(MYLIB)/RHash.jar:$(MYLIB)/cos.jar:$(MYLIB)/bullejb.jar
C3=$(MYLIB)/spy.jar:$(MYLIB)/postgresql.jar:$(MYLIB)/xml4j.jar
C4=$(MYLIB)/providerutil.jar:$(MYLIB)/jta-spec1_0_1.jar
CLASSPATH=$(C1):$(C2):$(C3):$(C4):../../..:.
SERVLETRUNNER=/home/dustin/lib/java/JSDK2.0/bin/servletrunner

JONAS_ROOT=$(HOME)/lib/java/jonas
JONAS=$(JONAS_ROOT)/bin/unix
GENDD=$(JONAS)/GenDD
GENIC=$(JONAS)/GenIC

SCP=scp
DEST=bleu.west.spy.net:/usr/local/apache/java
# DEST=170.1.69.194:/usr/local/apache/java

CLASSES=TestSB.class TestSBBean.class TestSBHome.class \
	TestSBClient.class

SER=TestSB.ser

GENCLASSES=BullTestSBBeanTestSBHome.class BullTestSBBeanTestSB.class \
	BullTestSBBeanTestSBHome_Skel.class BullTestSBBeanTestSBHome_Stub.class \
	BullTestSBBeanTestSB_Skel.class BullTestSBBeanTestSB_Stub.class

.SUFFIXES: .java .class .jar .txt .ser

all: $(CLASSES) $(SER) $(GENCLASSES)

genic: $(SER)
	env CLASSPATH=$(CLASSPATH) JONAS_ROOT=$(JONAS_ROOT) $(GENIC) -keepgenerated -d ../../.. $(SER)

$(GENCLASSES): genic

test: all
	env CLASSPATH=$(CLASSPATH) $(SERVLETRUNNER) -d $(PWD)

clean:
	rm -f $(CLASSES) $(SER) $(GENCLASSES)

$(SER): $(CLASSES) TestSB.txt

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -d ../../.. $<

.txt.ser:
	env CLASSPATH=$(CLASSPATH) JONAS_ROOT=$(JONAS_ROOT) $(GENDD) $<
