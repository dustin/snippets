// Copyright (c) 1999 Dustin Sallings
//
// $Id: WatchDog.java,v 1.3 2002/07/10 04:24:45 dustin Exp $

package net.spy;


/**
 * A simple watchdog timer.
 * <p>
 * *NOTE* this will cause entire JVM termination.  Use it where it makes
 * sense.
 */

public class WatchDog extends Thread {
	long lifetime=0;

	/**
	 * Constructs a WatchDog timer that will last for the given number of
	 * milliseconds.
	 */
	public WatchDog(long lifetime) {
		super();
		this.lifetime=lifetime;
		this.setDaemon(true);
		this.setName("WatchDog");
	}

	public void run() {
		try {
			sleep(lifetime);
		} catch(Exception e) {
			// We don't care yet.
		}
		System.err.println("Watchdog timer expired!!!");
		System.exit(-1);
	}
}
