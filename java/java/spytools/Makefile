# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
RMIC=$(JAVAHOME)/bin/rmic
JAVADOC=$(JAVAHOME)/bin/javadoc
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/DBCB.jar:$(MYLIB)/postgresql.jar
C2=$(MYLIB)/RHash.jar:$(MYLIB)/cos.jar
C3=$(MYLIB)/xml4j.jar:$(MYLIB)/oromatcher.jar
CLASSPATH=$(C1):$(C2):$(C3):.

CLASSES=net/spy/SpyConfig.class net/spy/SpyDB.class \
	net/spy/SpyLog.class net/spy/SpyLogEntry.class \
	net/spy/SpyToker.class net/spy/SpyLogFlusher.class \
	net/spy/SpyConfigReader.class net/spy/net/SNPP.class \
	net/spy/net/HTTPFetch.class net/spy/SpyUtil.class net/spy/Syslog.class \
	net/spy/util/ImageData.class net/spy/rmi/ImageServer.class \
	net/spy/rmi/ImageServerImpl.class net/spy/rmi/RObject.class \
	net/spy/rmi/ImageServerImpl_Stub.class \
	net/spy/rmi/ImageServerImpl_Skel.class net/spy/rmi/RObjectImpl.class \
	net/spy/rmi/RObjectImpl_Stub.class net/spy/rmi/RObjectImpl_Skel.class \
	net/spy/rmi/DoBindings.class net/spy/RHash.class \
	net/spy/temperature/SpyTemp.class net/spy/SpyRunner.class \
	net/spy/nmap/NmapEntry.class net/spy/nmap/NmapPort.class \
	net/spy/nmap/NmapParser.class net/spy/WatchDog.class \
	net/spy/info/Info.class \
	net/spy/info/Stocks.class net/spy/info/StockQuote.class \
	net/spy/info/eBay.class \
	net/spy/info/PackageInfo.class \
	net/spy/info/FedEx.class \
	net/spy/info/Traffic.class \
	net/spy/info/FlightTracker.class \
	net/spy/info/BigWords.class \
	net/spy/info/UPS.class net/spy/info/Weather.class


PACKAGES=net.spy net.spy.net net.spy.rmi net.spy.temperature net.spy.util \
	net.spy.info

FDEST=/home/dustin/lib/java
SCP=scp
SDEST=bleu.west.spy.net:/usr/local/apache/java

.SUFFIXES: .java .class .jar

all: spy.jar

docs:
	$(JAVADOC) -public -d doc $(PACKAGES)

spy.jar: $(CLASSES)
	$(JAR) cv0f $@ $(CLASSES)

install: all
	cp spy.jar $(FDEST)
	$(SCP) spy.jar $(SDEST)

clean:
	rm -f $(CLASSES) spy.jar

net/spy/rmi/ImageServerImpl_Skel.class: \
	net/spy/rmi/ImageServerImpl.class
	$(RMIC) -d . net.spy.rmi.ImageServerImpl

net/spy/rmi/ImageServerImpl_Stub.class: \
        net/spy/rmi/ImageServerImpl_Skel.class

net/spy/rmi/RObjectImpl_Skel.class: \
	net/spy/rmi/RObjectImpl.class
	$(RMIC) -d . net.spy.rmi.RObjectImpl

net/spy/rmi/RObjectImpl_Stub.class: \
	net/spy/rmi/RObjectImpl_Skel.class

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) $<
