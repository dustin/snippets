//
// $Id: SNPPPoolAble.java,v 1.3 2001/02/07 06:31:42 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;
import net.spy.net.SNPP;

/**
 * PoolAble object for containing a SNPP object.
 */

public class SNPPPoolAble extends PoolAble {

	public SNPPPoolAble(Object the_object) {
		super(the_object);
	}

	public SNPPPoolAble(Object the_object, long max_age) {
		super(the_object, max_age);
	}

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
			available=false;
		}
		return(ret);
	}
}
