// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Promise.java,v 1.3 2002/08/18 07:32:14 dustin Exp $

package net.spy.util;

/**
 * A promise, continuation style code for java.
 */
public abstract class Promise extends Object {

	private Object rv=null;
	private boolean hasRun=false;
	private BrokenPromiseException bpe=null;

	/**
	 * Get an instance of Promise.
	 */
	public Promise() {
		super();
	}

	/**
	 * Get the object.
	 *
	 * @return the Object we were promised.
	 *
	 * @exception BrokenPromiseException if there's a problem getting what
	 * we were promised.
	 */
	public final Object getObject() throws BrokenPromiseException {
		if(hasRun == false) {
			try {
				rv=execute();
				hasRun=true;
			} catch(BrokenPromiseException bpe) {
				this.bpe=bpe;
				throw bpe;
			}
		}

		// If there was a broken promise, toss it.
		if(bpe!=null) {
			throw bpe;
		}

		return(rv);
	}

	/**
	 * Print me.
	 */
	public String toString() {
		String rvs=null;

		if(hasRun) {
			if(rv!=null) {
				rvs="Promise {" + rv.toString() + "}";
			} else {
				if(bpe!=null) {
					rvs="Broken Promise {" + bpe.toString() + "}";
				} else {
					rvs="Promise {null}";
				}
			}
		} else {
			rvs="Promise {not yet executed}";
		}

		return(rvs);
	}

	/**
	 * Do the actual work required to get the Object we're promised.
	 */
	protected abstract Object execute() throws BrokenPromiseException;

}

