//
// $Id: PooledObject.java,v 1.3 2000/07/04 05:40:45 dustin Exp $

package net.spy.pool;

/**
 * Pooled object return package.  This object primarily exists as a means
 * for having a safe way to have access to PoolAble objects.  Anyone using
 * a PooledObject will not be able to retrieve the object that was pooled
 * after checking it back in, and it makes it safe to forget to check an
 * object back in on occasion.
 */
public class PooledObject extends Object {

	protected PoolAble p=null;

	protected boolean _debug=false;

	/**
	 * Get a new PooledObject containing the given PoolAble
	 */
	public PooledObject(PoolAble p) {
		super();
		this.p=p;
		p.checkOut();
	}

	/**
	 * Get the object we just checked out.
	 *
	 * @throws PoolException if a problem occurs
	 */
	public Object getObject() throws PoolException {
		return(p.getObject());
	}

	/**
	 * Find out if the object is alive
	 *
	 * @return true if the object is alive
	 */
	public boolean alive() {
		return(p.alive());
	}

	/**
	 * Manually check the object back in.
	 */
	public void checkIn() {
		p.checkIn();
		p=null;
	}

	/**
	 * Get the objectID for the pool object we have checked out.
	 *
	 * @return the object ID
	 */
	public int getObjectID() {
		return(p.getObjectID());
	}

	/**
	 * Finalization will check-in any checked-out object that has not
	 * already been checked in.
	 */
	public void finalize() {
		if(p!=null) {
			debug("###### Finalization checking in object " + p.getObjectID());
			p.checkIn();
		}
	}

	protected void debug(String what) {
		if(_debug) {
			System.out.println(what);
		}
	}
}
