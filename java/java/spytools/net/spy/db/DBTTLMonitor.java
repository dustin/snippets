// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: DBTTLMonitor.java,v 1.2 2002/07/10 04:25:22 dustin Exp $

package net.spy.db;

import java.util.Iterator;
import java.util.Vector;

/**
 * Monitors TTLs.
 */
public class DBTTLMonitor extends Thread {

	private Vector ttls=null;

	private static final long NAPTIME=5000;
	private long lastAddition=0;
	private static final long MAX_QUIESCENCE=300000;

	/**
	 * Get an instance of DBTTLMonitor.
	 */
	public DBTTLMonitor() {
		super();
		ttls=new Vector();
		lastAddition=System.currentTimeMillis();
		setName("DB TTL Monitor");
		setDaemon(true);
		start();
	}

	/**
	 * Add a new TTL to the list we're monitoring.
	 */
	public void addTTL(DBTTL ttl) {
		lastAddition=System.currentTimeMillis();
		synchronized(ttls) {
			ttls.addElement(ttl);
		}
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();
		sb.append(super.toString());
		sb.append(" - Outstanding TTLs:  ");
		sb.append(ttls.size());
		return(sb.toString());
	}

	private boolean shouldIKeepRunning() {
		long now=System.currentTimeMillis();
		return((lastAddition+MAX_QUIESCENCE) > now);
	}

	/**
	 * Monitor the TTLs.
	 */
	public void run() {
		while(shouldIKeepRunning()) {
			synchronized(ttls) {
				for(Iterator i=ttls.iterator(); i.hasNext(); ) {
					DBTTL ttl=(DBTTL)i.next();
					ttl.report();
					if(ttl.isClosed()) {
						i.remove();
					} // closed
				} // Iterator
			} // Lock

			try {
				sleep(NAPTIME);
			} catch(InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

}
