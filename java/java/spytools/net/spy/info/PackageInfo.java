// Copyright (c) 2000  Dustin Sallings
//
// $Id: PackageInfo.java,v 1.2 2001/02/07 06:31:16 dustin Exp $

package net.spy.info;

/**
 * Abstract class for Info classes that describe shipping information.
 */
public abstract class PackageInfo extends Info {
	protected boolean delivered=false;

	public boolean isDelivered() {
		return(delivered);
	}
}
