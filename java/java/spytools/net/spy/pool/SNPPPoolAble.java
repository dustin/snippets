//
// $Id: SNPPPoolAble.java,v 1.2 2000/07/25 07:11:25 dustin Exp $

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
		available=false;
		try {
			SNPP snpp=(SNPP)the_object;
			snpp.close();
		} catch(Exception e) {
			System.err.println("Error discarding SNPP object:  " + e);
			e.printStackTrace();
		}
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
			SNPP snpp=(SNPP)the_object;
			snpp.cmd("RESET");
			ret=true;
		} catch(Exception e) {
			// Turn off availability
			available=false;
		}
		return(ret);
	}
}
