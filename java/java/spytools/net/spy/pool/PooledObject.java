//
// $Id: PooledObject.java,v 1.2 2000/07/03 07:23:26 dustin Exp $

package net.spy.pool;

public class PooledObject extends Object {

	protected PoolAble p=null;

	protected boolean _debug=false;

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
