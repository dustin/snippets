//
// $Id: PoolContainer.java,v 1.1 2000/07/01 09:44:30 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

import net.spy.SpyConfig;

public class PoolContainer extends Object {
	protected Vector pool=null;
	protected SpyConfig conf=null;
	protected String name=null;
	protected PoolFiller filler=null;
	protected int _min_objects=-1;
	protected int _max_objects=-1;
	protected int _current_objects=0;
	protected int _object_id=0;

	public PoolContainer(String name, PoolFiller pf, SpyConfig conf)
		throws PoolException {
		super();
		this.conf=conf;
		this.name=name;
		this.filler=pf;

		initialize();
	}

	public synchronized PoolAble getObject() throws PoolException {
		PoolAble ret=null;

		// We'll try up to three times to get an object from the pool
		for(int retry=0; ret==null && retry<3; retry++) {

			// Find the next available object.
			for(Enumeration e=pool.elements();
				ret==null && e.hasMoreElements(); ) {

				PoolAble p=(PoolAble)e.nextElement();

				// If it's not checked out, we have our man!
				if(!p.checkedOut()) {
					// Since we got one from the pool, we want to move it
					// to the end of the vector.
					ret=p;
				}
			} // Flipping through the current pool

			try {
				System.out.println("*** No free entries in pool, sleeping ***");
				// Wait a second if the pool is full, in case it frees up
				Thread.sleep(1000);
			} catch(Exception e) {
				// Things just go faster.
			}
		}// Retries for an object in the existing pool.

		// If the above didn't get us an object, we'll resort to getting a
		// new one.
		if(ret==null) {
			// OK, got nothing from the pool, in a desperate attempt, we'll
			// be grabbing a new object.
			ret=getNewObject();
		}

		// Check it out.
		ret.checkOut();

		// OK, let's stick it at the end of the vector (may already be, but
		// you know...) so that it's one of the last we check for next time.
		pool.removeElement(ret);
		pool.addElement(ret);

		return(ret);
	}

	public void dumpPool() {
		for(Enumeration e=pool.elements(); e.hasMoreElements();) {
			System.out.println("\t" + e.nextElement());
		}
	}

	protected void initialize() throws PoolException {
		pool=new Vector();

		// Get the min and max args.
		_min_objects=getPropertyInt("min", 1);
		_max_objects=getPropertyInt("max", 10);

		// Populate with the minimum number of objects.
		for(int i=0; i<_min_objects; i++) {
			PoolAble p=getNewObject();
		}
	}

	protected PoolAble getNewObject() throws PoolException {
		PoolAble p=null;
		// Don't add an object if we're at capacity.
		if(_current_objects<_max_objects) {
			System.out.println("*** Getting a new object in the "
				+ name + " pool, currently have " + _current_objects
				+ ". ***");
			p=filler.getObject();
			_current_objects++;
			p.setObjectID(_object_id);
			_object_id++;
			pool.addElement(p);
		} else {
			throw new PoolException("Cannot create another object in the pool");
		}
		return(p);
	}

	protected int getPropertyInt(String what, int def) {
		return(conf.getInt(name + "." + what, def));
	}

	protected String getProperty(String what, String def) {
		return(conf.get(name + "." + what, def));
	}

	protected String getProperty(String what) {
		return(conf.get(name + "." + what));
	}
}
