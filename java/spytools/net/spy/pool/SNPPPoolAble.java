//
// $Id: SNPPPoolAble.java,v 1.6 2002/07/10 05:42:06 dustin Exp $

package net.spy.pool;

import net.spy.net.SNPP;

/**
 * PoolAble object for containing a SNPP object.
 */
public class SNPPPoolAble extends PoolAble {

	/**
	 * Get an instance of SNPPPoolAble.
	 */
	public SNPPPoolAble(Object theObject, int poolHash) {
		super(theObject, poolHash);
	}

	/**
	 * Get an instance of SNPPPoolAble.
	 */
	public SNPPPoolAble(Object theObject, long maxAge, int poolHash) {
		super(theObject, maxAge, poolHash);
	}

	/**
	 * @see PoolAble
	 */
	public void discard() {
		try {
			SNPP snpp=(SNPP)intGetObject();
			if(snpp!=null) {
				snpp.close();
			}
		} catch(Exception e) {
			System.err.println("Error discarding SNPP object:  " + e);
			e.printStackTrace();
		}
		super.discard();
	}

	/**
	 * Find out of the SNPPPoolAble represents a usable object.  This is
	 * done via an SNPP reset.
	 *
	 * @return true if the object will be usable
	 */
	public boolean isAlive() {
		boolean ret=false;
		try {
			SNPP snpp=(SNPP)intGetObject();
			if(snpp!=null) {
				snpp.cmd("RESET");
				ret=true;
			}
		} catch(Exception e) {
			// Turn off availability
			setUnavailable();
		}
		return(ret);
	}
}
