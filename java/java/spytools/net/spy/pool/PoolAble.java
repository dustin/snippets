//
// $Id: PoolAble.java,v 1.3 2000/07/03 07:57:20 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

public class PoolAble extends Object {
	protected int object_id=-1;
	protected boolean checked_out=false;
	protected boolean available=true;
	protected Object the_object=null;
	protected long max_age=0;
	protected long start_time=0;

	public PoolAble(Object the_object) {
		super(); // thanks for asking.
		this.the_object=the_object;
		start_time=System.currentTimeMillis();
	}

	public PoolAble(Object the_object, long max_age) {
		super(); // thanks for asking.
		this.the_object=the_object;
		this.max_age=max_age;
		start_time=System.currentTimeMillis();
	}

	/**
	 * Find out of the PoolAble represents a usable object.  Objects
	 * extending PoolAble should implement alive() methods for their
	 * particular needs.
	 *
	 * @return true if the object will be usable
	 */
	public boolean alive() {
		return(true);
	}

	/**
	 * Get the object we're pooling.
	 *
	 * @return the object.
	 *
	 * @throws PoolException if something bad happens (i.e. the object is
	 * not checked out)
	 */
	public Object getObject() throws PoolException {
		if(!checked_out) {
			throw new PoolException("This PoolAble has not been checked out.");
		}
		return(the_object);
	}

	/**
	 * Set the internal object ID.  This probably shouldn't be called
	 * outside of the pool container.
	 *
	 * @param id ObjectId of this object.
	 */
	public void setObjectID(int id) {
		this.object_id=id;
	}

	/**
	 * Get the object ID of this object.
	 *
	 * @return the object ID
	 */
	public int getObjectID() {
		return(object_id);
	}

	/**
	 * Check the object back in.  The PoolAble will not be usable again
	 * until it's checked back out from the pooler.
	 */
	public synchronized void checkIn() {
		checked_out=false;
		if(max_age==0) {
			available=true;
		} else {
			long current_time=System.currentTimeMillis();
			if(current_time-start_time < max_age) {
				available=true;
			} else {
				// This should already be the case, but let's make sure.
				available=false;
			}
		}
	}

	/**
	 * Check the object out.  Do *NOT* call this directly, bad things will
	 * happen.
	 */
	public synchronized void checkOut() {
		checked_out=true;
		available=false;
	}

	/**
	 * Find out if the object is checked out.
	 */
	public synchronized boolean isCheckedOut() {
		return(checked_out);
	}

	/**
	 * Find out if the object is available for a requestor
	 */
	public synchronized boolean isAvailable() {
		return(available);
	}

	/**
	 * Find out if an object is prunable
	 *
	 * @return 0 if not available, 1 if prunable, and 2 if a pruning is
	 * required.
	 */
	public synchronized int pruneStatus() {
		int ret=0;

		// If it's not checked out, we can prune it.
		if(!checked_out) {
			ret++;
			// If it's not checked out, and it's not available, we *need*
			// to prune it.
			if(available) {
				ret++;
			}
		}

		return(ret);
	}

	/**
	 * Return a string representation of this object.
	 *
	 * @return a string representation of this object.
	 */
	public String toString() {
		String out=null;
		if(isCheckedOut()) {
			out="PoolAble " + object_id + " is checked out ";
		} else {
			out="PoolAble " + object_id + " is not checked out ";
		}
		if(max_age>0) {
			out+="expires " + new Date(start_time + max_age);
		}
		return(out);
	}
}
