// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: KeyStore.java,v 1.1 2002/08/23 07:46:22 dustin Exp $

package net.spy.db;

import java.math.BigDecimal;

/**
 * Store a range of primary keys.
 */
public class KeyStore extends Object {

	private BigDecimal start=null;
	private BigDecimal end=null;
	private BigDecimal current=null;

	private BigDecimal ONE=new BigDecimal(1);

	/**
	 * Get an instance of KeyStore.
	 */
	public KeyStore(BigDecimal start, BigDecimal end) {
		super();
		this.start=start;
		this.current=start;
		this.end=end;
	}

	/**
	 * String me.
	 */
	public String toString() {
		return("KeyStore from " + start + " to " + end);
	}

	/**
	 * Get the next key.
	 *
	 * @return the next available key
	 * @throws OverDrawnException if there are no keys left in this store
	 */
	public synchronized BigDecimal nextKey() throws OverDrawnException {
		BigDecimal rv=current;
		// Make sure we don't run out
		if(current.compareTo(end) > 0) {
			throw new OverDrawnException();
		}
		// increment
		current=current.add(ONE);
		return(rv);
	}

}
