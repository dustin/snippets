//
// $Id: PoolAble.java,v 1.24 2002/08/21 00:53:11 dustin Exp $

package net.spy.pool;

import java.util.Date;

/**
 * PoolAble is the object container that is used to store objects in the
 * pool.
 *
 * <p>
 *
 * The system property net.spy.pool.debug can be set to a file where the
 * debugging output will go.
 */

public abstract class PoolAble extends Object {
	private int objectId=-1;
	private boolean checkedOut=false;
	private Object theObject=null;
	private long maxAge=0;
	private long startTime=0;
	private String poolName=null;
	private PoolDebug pooldebug=null;
	private int checkouts=0;
	private int checkins=0;
	private int poolHash=0;

	/**
	 * Minimum value returned from pruneStatus() if we may clean the object.
	 */
	public static final int MAY_CLEAN=1;
	/**
	 * Minimum value returned from pruneStatus() if we must clean the object.
	 */
	public static final int MUST_CLEAN=2;

	private boolean available=true;

	/**
	 * Get a PoolAble representation for an object.
	 */
	public PoolAble(Object theObject, int poolHash) {
		super(); // thanks for asking.
		this.theObject=theObject;
		this.poolHash=poolHash;
		startTime=System.currentTimeMillis();
		debug("New object");
	}

	/**
	 * Get a PoolAble representation for an object, including a given
	 * maximum lifetime the object can have.
	 *
	 * @param maxAge the amount of time, in milliseconds, that the object
	 * will be valid.  Objects will not be checked out if they are older
	 * than their maximum lifetime.
	 */
	public PoolAble(Object theObject, long maxAge, int poolHash) {
		super(); // thanks for asking.
		this.theObject=theObject;
		this.maxAge=maxAge;
		this.poolHash=poolHash;
		startTime=System.currentTimeMillis();
		debug("New object.");
	}

	// Get the debug name
	private String debugName() {
		StringBuffer sb=new StringBuffer(64);
		sb.append("PoolAble ");
		sb.append(objectId);
		sb.append(" for ");
		sb.append(Integer.toHexString(poolHash));
		return(sb.toString());
	}

	/**
	 * Set the maximum age of this PoolAble.
	 */
	public void setMaxAge(long to) {
		this.maxAge=to;
	}

	/**
	 * Find out of the PoolAble represents a usable object.  Objects
	 * extending PoolAble should implement isAlive() methods for their
	 * particular needs.
	 * <p>
	 * Objects implementing isAlive() <i>should</i> turn off object
	 * availability if they determine the object no longer isAlive().
	 *
	 * @return true if the object will be usable
	 */
	public synchronized boolean isAlive() {
		return(true);
	}

	/**
	 * Get the object we're pooling.
	 *
	 * @return the object.
	 *
	 * @exception PoolException if something bad happens (i.e. the object is
	 * not checked out)
	 */
	public synchronized Object getObject() throws PoolException {
		if(!checkedOut) {
			throw new PoolException("This PoolAble has not been checked out.");
		}
		return(theObject);
	}

	/**
	 * Internal version of getObject().  Returns regardless of whether the
	 * object has been checked out.
	 */
	protected Object intGetObject() {
		return(theObject);
	}

	/**
	 * Set the internal object ID.  This probably shouldn't be called
	 * outside of the pool container.
	 *
	 * @param id ObjectId of this object.
	 */
	public void setObjectID(int id) {
		this.objectId=id;
	}

	/**
	 * Set the pool name this thing sits in.  This is for debugging, but
	 * it's useful information, nonetheless.
	 */
	public void setPoolName(String to) {
		this.poolName=to;
	}

	/**
	 * Get the object ID of this object.
	 *
	 * @return the object ID
	 */
	public int getObjectID() {
		return(objectId);
	}

	/**
	 * Activate this PoolAble object.
	 */
	public void activate() {
		debug("Activated.");
	}

	/**
	 * Check the object back in.  The PoolAble will not be usable again
	 * until it's checked back out from the pooler.
	 * <p>
	 * checkIn also does some checks such as making sure the item is not
	 * too old, and that it is still alive.
	 */
	public synchronized void checkIn() {
		checkedOut=false;
		checkins++;

		// At this point, set the availability based on whether this object
		// is expired.
		available=!isExpired();

		// Also, make sure the thing's alive.
		if(!isAlive()) {
			available=false;
		}

		debug("Checked in.");
	}

	/**
	 * Mark this object as available.
	 */
	protected void setAvailable() {
		available=true;
	}

	/**
	 * Mark this object as unavailable.
	 */
	protected void setUnavailable() {
		available=false;
	}

	// Returns true if this should be invalidated based on the time
	private boolean isExpired() {
		boolean rv=true;

		if(maxAge==0) {
			rv=false;
		} else {
			long currentTime=System.currentTimeMillis();
			if(currentTime-startTime < maxAge) {
				rv=false;
			}
		}
		return(rv);
	}

	/**
	 * Check the object out.  Called from the PoolContainer
	 */
	public synchronized void checkOut() {
		checkouts++;
		checkedOut=true;
		available=false;
		debug("Checked out.");
	}

	/**
	 * Find out if the object is checked out.
	 */
	public synchronized boolean isCheckedOut() {
		return(checkedOut);
	}

	/**
	 * Find out if the object is available for a requestor
	 */
	public synchronized boolean isAvailable() {
		// If it currently believes it's available, but it's expired, make
		// it unavailable.
		if(available) {
			if(isExpired()) {
				available=false;
			}
		}
		return(available);
	}

	/**
	 * Find out if an object is prunable
	 *
	 * @return 0 if not available, 1 if we may clean, greater than one if
	 * we must clean.
	 */
	public synchronized int pruneStatus() {
		int ret=0;

		// If it's not checked out, we can prune it.
		if(!checkedOut) {
			ret++;
			// If it's not checked out, and it's not available, we *need*
			// to prune it.
			if(!isAvailable()) {
				ret++;
			}

			// If it's not alive, we don't want it.
			if(!isAlive()) {
				ret++;
			}
		}

		return(ret);
	}

	/**
	 * Discard the object.  Anything that extends from this class needs to
	 * call super.discard() when it's done.
	 */
	public void discard() {
		debug("Discard called.");
		available=false;
		theObject=null;
	}

	/**
	 * Debugging info.
	 */
	protected final void debug(String msg) {
		if(pooldebug==null) {
			pooldebug=new PoolDebug();
		}

		String classname=getClass().getName();
		String objectClassname="n/a";
		if(theObject!=null) {
			objectClassname=theObject.getClass().getName();
		}

		String tmsg= "Poolable=" + classname + ", oid=" + objectId
			+ " in " + poolName + ", object=" + objectClassname + ":  " + msg;

		pooldebug.debug(tmsg);
	}

	/**
	 * Return a string representation of this object.
	 *
	 * @return a string representation of this object.
	 */
	public synchronized String toString() {
		StringBuffer out=new StringBuffer(128);
		out.append(debugName());
		if(isCheckedOut()) {
			out.append(" is checked out");
		} else {
			out.append(" is not checked out");
		}
		out.append(" (o=" + checkouts + ", i=" + checkins + ")");
		if(maxAge>0) {
			out.append(" expires " + new Date(startTime + maxAge));
		}
		if(!isAvailable()) {
			out.append(" (not available)");
		}
		return(out.toString());
	}
}
