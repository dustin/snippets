# Makefile for RHash, remote object server stuff.

JAVAHOME=/usr/java
JAR=$(JAVAHOME)/bin/jar
# JAVAC=$(JAVAHOME)/bin/javac
JAVAC=jikes +P
JAVA=$(JAVAHOME)/bin/java
RMIC=$(JAVAHOME)/bin/rmic
JAVADOC=$(JAVAHOME)/bin/javadoc
COMPILE=$(JAVAC)
MYLIB=$(HOME)/lib/java
C1=$(MYLIB)/jsdk.jar:$(MYLIB)/jdbc2_0-stdext.jar
C2=$(MYLIB)/jndi.jar
S=/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar
CLASSPATH=$(C1):$(C2):$S:.

CLASSES=net/spy/SpyConfig.class net/spy/SpyDB.class \
	net/spy/log/SpyLog.class \
	net/spy/log/SpyLogEntry.class \
	net/spy/log/SpyLogFlusher.class \
	net/spy/log/SpyLogQueue.class \
	net/spy/log/SpyMessage.class \
	net/spy/log/MCastLog.class \
	net/spy/log/MCastListener.class \
	net/spy/log/MessageOutputStream.class \
	net/spy/SpyToker.class \
	'net/spy/SpyConfig$$SpyConfigReader.class' net/spy/net/SNPP.class \
	net/spy/net/IOTimeoutException.class \
	net/spy/net/TimedSocket.class \
	net/spy/net/HTTPFetch.class net/spy/SpyUtil.class net/spy/Syslog.class \
	net/spy/rmi/RObject.class \
	net/spy/rmi/RObjectImpl.class \
	net/spy/rmi/RObjectImpl_Stub.class net/spy/rmi/RObjectImpl_Skel.class \
	net/spy/RHash.class \
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
	net/spy/info/UPS.class net/spy/info/Weather.class \
	net/spy/util/SpyLDIF.class \
	net/spy/util/SpySort.class \
	net/spy/util/SpyComparable.class \
	net/spy/util/SpyDateComparable.class \
	net/spy/util/SpyDoubleComparable.class \
	net/spy/util/SpyIntegerComparable.class \
	net/spy/util/SpyLDIFReader.class \
	net/spy/util/InterfaceImplementor.class \
	net/spy/util/QuickRun.class \
	net/spy/util/Debug.class \
	net/spy/util/Base64.class \
	net/spy/util/Base64OutputStream.class \
	net/spy/util/Base64InputStream.class \
	net/spy/util/UUInputStream.class \
	net/spy/util/TimeStampedHash.class \
	net/spy/util/Digest.class \
	net/spy/util/SPGen.class \
	'net/spy/util/SPGen$$Parameter.class' \
	net/spy/pool/ObjectPool.class \
	'net/spy/pool/ObjectPool$$ObjectPoolCleaner.class' \
	net/spy/util/NestedException.class \
	net/spy/util/NestedRuntimeException.class \
	net/spy/util/BrokenPromiseException.class \
	net/spy/util/Promise.class \
	net/spy/util/PwGen.class \
	net/spy/util/ThreadPool.class \
	net/spy/util/TimeSpan.class \
	net/spy/util/RingBuffer.class \
	net/spy/pool/PoolAble.class \
	net/spy/pool/PoolContainer.class \
	net/spy/pool/PoolException.class \
	net/spy/pool/PoolFiller.class \
	net/spy/pool/PooledObject.class \
	net/spy/pool/JDBCPoolFiller.class \
	net/spy/pool/JDBCPoolAble.class \
	net/spy/pool/SNPPPoolFiller.class \
	net/spy/pool/SNPPPoolAble.class \
	net/spy/pool/PoolDebug.class \
	net/spy/cache/SpyCache.class \
	'net/spy/cache/SpyCache$$SpyCacheCleaner.class' \
	'net/spy/cache/SpyCache$$SpyCacheItem.class' \
	net/spy/cache/CacheClearRequestListener.class \
	net/spy/db/CachePreparedStatementStub.class \
	net/spy/db/CachedResultSetStub.class \
	net/spy/db/CachePreparedStatement.class \
	net/spy/db/CachedResultSet.class \
	net/spy/db/SpyCacheDB.class \
	net/spy/db/DBNull.class \
	net/spy/db/DBSP.class \
	net/spy/db/DBSQL.class \
	net/spy/db/TypeNames.class \
	net/spy/db/DBTTL.class \
	net/spy/db/DBTTLMonitor.class \
	net/spy/cron/TimeIncrement.class \
	net/spy/cron/Job.class \
	net/spy/cron/JobQueue.class \
	net/spy/cron/Cron.class \
	net/spy/cron/MainJob.class \
	net/spy/cron/FileJobQueue.class \
	net/spy/aaa/AuthException.class \
	net/spy/aaa/AuthUser.class \
	net/spy/aaa/Authable.class \
	net/spy/aaa/AuthableImpl.class \
	net/spy/aaa/Authenticator.class \
	net/spy/aaa/SimpleAuthUser.class \
	net/spy/aaa/SpyPermission.class \
	net/spy/aaa/SpyPermissionCollection.class \
	net/spy/aaa/SpyPolicy.class \
	net/spy/aaa/SpySecurityException.class \
	net/spy/aaa/Token.class \
	net/spy/aaa/SpySecurityManager.class \
	net/spy/aaa/SpySecurityPermissionException.class

	# net/spy/loader/ClassReLoader.class

# Extra stuff to be cleaned
GENERATED=net/spy/db/CachePreparedStatement.java \
	net/spy/db/CachedResultSet.java

PACKAGES=net.spy net.spy.net net.spy.rmi net.spy.temperature \
	net.spy.info net.spy.pool net.spy.log net.spy.cache net.spy.util \
	net.spy.db net.spy.cron net.spy.aaa

FDEST=/home/dustin/lib/java
SCP=scp
SDEST=bleu.west.spy.net:/usr/local/apache/java

.SUFFIXES: .java .class .jar

all: $(CLASSES)

doc: $(CLASSES)
	$(JAVADOC) -d doc \
		-windowtitle "Spy.jar Documentation" \
		-doctitle "spy.jar Documentation" \
		-linkoffline http://java.sun.com/products/jdk/1.3/docs/api/ \
			http://java.sun.com/j2se/1.3/docs/api/ \
		$(PACKAGES)

jar: spy.jar

spy.jar: $(CLASSES)
	$(JAR) cv0f $@ `find net -name "*.class"`

install: all
	cp spy.jar $(FDEST)
	$(SCP) spy.jar $(SDEST)

clean:
	rm -f $(CLASSES) $(GENERATED) spy.jar

net/spy/rmi/RObjectImpl_Skel.class: \
	net/spy/rmi/RObjectImpl.class
	$(RMIC) -d . net.spy.rmi.RObjectImpl

net/spy/rmi/RObjectImpl_Stub.class: \
	net/spy/rmi/RObjectImpl_Skel.class

'net/spy/cache/SpyCache$$SpyCacheItem.class': net/spy/cache/SpyCache.class

'net/spy/cache/SpyCache$$SpyCacheCleaner.class': net/spy/cache/SpyCache.class

'net/spy/pool/ObjectPool$$ObjectPoolCleaner.class': \
	net/spy/pool/ObjectPool.class

'net/spy/SpyConfig$$SpyConfigReader.class': net/spy/SpyConfig.class

'net/spy/util/SPGen$$Parameter.class': net/spy/util/SPGen.class

# Generated CachedResultSet.java (to class)
net/spy/db/CachedResultSet.java: \
	net/spy/util/InterfaceImplementor.class \
	net/spy/db/CachedResultSetStub.class
	$(JAVA) net.spy.util.InterfaceImplementor \
		-interface java.sql.ResultSet \
		-superclass net.spy.db.CachedResultSetStub \
		-outputclass net.spy.db.CachedResultSet

net/spy/db/CachePreparedStatementStub.class: net/spy/db/CachedResultSet.class

# Generated CachePreparedStatement.java (to class)
net/spy/db/CachePreparedStatement.java: \
	net/spy/util/InterfaceImplementor.class \
	net/spy/db/CachePreparedStatementStub.class
	$(JAVA) net.spy.util.InterfaceImplementor \
		-interface java.sql.PreparedStatement \
		-superclass net.spy.db.CachePreparedStatementStub \
		-outputclass net.spy.db.CachePreparedStatement

.java.class:
	env CLASSPATH=$(CLASSPATH) $(COMPILE) -deprecation $<
