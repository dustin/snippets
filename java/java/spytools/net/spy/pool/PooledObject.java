//
// $Id: PooledObject.java,v 1.1 2000/07/01 11:05:58 dustin Exp $

package net.spy.pool;

public class PooledObject extends Object {

	protected PoolAble p=null;

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

	// Auto-checkin
	public void finalize() {
		if(p!=null) {
			System.out.println("###### Finalization checking in object "
				+ p.getObjectID());
			p.checkIn();
		}
	}
}
