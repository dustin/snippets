//
// $Id: PoolAble.java,v 1.1 2000/07/01 09:44:29 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

public class PoolAble extends Object {
	protected int object_id=-1;
	protected boolean checked_out=false;
	protected Object the_object=null;

	public PoolAble(Object the_object) {
		super(); // thanks for asking.
		this.the_object=the_object;
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
	public void checkIn() {
		checked_out=false;
	}

	/**
	 * Check the object out.  Do *NOT* call this directly, bad things will
	 * happen.
	 */
	public void checkOut() {
		checked_out=true;
	}

	/**
	 * Find out if the object is checked out.
	 */
	public boolean checkedOut() {
		return(checked_out);
	}

	/**
	 * Return a string representation of this object.
	 *
	 * @return a string representation of this object.
	 */
	public synchronized String toString() {
		String out=null;
		if(checked_out) {
			out="PoolAble " + object_id + " is checked out.";
		} else {
			out="PoolAble " + object_id + " is not checked out.";
		}
		return(out);
	}
}
