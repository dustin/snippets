# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr
JAR=$(JAVAHOME)/bin/jar
JAVAC=$(JAVAHOME)/bin/javac
JAVA=$(JAVAHOME)/bin/java
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/postgresql.jar:$(MYLIB)/spy.jar
CLASSPATH=$(C1):.

CLASSES=net/spy/temperature/Loader.class net/spy/temperature/TempSet.class \
	net/spy/temperature/TemperatureConf.class \
	net/spy/temperature/Sensor.class \
	net/spy/temperature/ReportBean.class

FDEST=/home/dustin/lib/java

.SUFFIXES: .java .class .jar

all: $(CLASSES)

test: all
	env CLASSPATH=$(CLASSPATH) $(JAVA) net.spy.temperature.ReportBean

install: all

clean:
	rm -f $(CLASSES)

.java.class:
	env CLASSPATH=$(CLASSPATH) $(JAVAC) -deprecation $<
